
/*
CREATE PROCEDURE [dbo].[WebS_OrdenDetalle_V09_sup]
 @Id_Num_Orden INT  
AS
*/


--DECLARE @Id_Num_Orden INT = 'NumeroOrden'

DECLARE @Id_Num_Orden INT = 533093122

 
  
-- *********************************************************************  
-- 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1  
-- Primer Record Set. Detalle de la Orden.  
-- 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1 - 1  
-- Revisa si el cliente tiene un Flete gratis que aplicar en esta orden.  
-- *********************************************************************  


declare @ueType varchar(20) = null;  
declare @CreatedBy varchar(25) = null;  
declare @Id_Num_UN SMALLINT
declare @Bit_RegAut BIT
declare @Ind_CobAut TINYINT
declare @Ind_CobAutFyV TINYINT
declare @Id_Num_UnInfo SMALLINT
declare @Id_Num_UnInfoFYV SMALLINT
declare @Id_Num_Orden16 VARCHAR(16)
declare @IsPickingManual bit=0;

SELECT	@Bit_RegAut		= 0
SELECT	@Id_Num_UnInfo	= 63 -- TYC Cob Aut en PVS
SELECT	@Id_Num_UnInfoFyV = 64 -- TYC Cob Aut de FyV en PVS
SELECT	@Id_Num_Orden16 = RIGHT('0000000000000000' + CONVERT(VARCHAR,@Id_Num_Orden), 16)

set @ueType = ISNULL((SELECT LTRIM(RTRIM(UeType)) FROM OrderFacts_UE WHERE ORDERNO = @Id_Num_Orden),'');  
set @CreatedBy = ISNULL((SELECT CreatedBy FROM OrderFacts_UE WHERE ORDERNO = @Id_Num_Orden),'');  
set @IsPickingManual = ISNULL((SELECT IsPickingManual FROM OrderFacts_UE WHERE ORDERNO = @Id_Num_Orden),0);  

DECLARE		@T		TABLE
	(
		 Id_Num_CodBarra		NUMERIC(14,0)	PRIMARY KEY
		,Id_Num_Orden			INT
		,Desc_Art				CHAR(255)
		,Cant_Surtida			decimal(14,3)
		,Prec_Normal			money
		,Prec_Oferta			money
		,Bit_Redondeo			BIT
		,Bit_SelMejorPrecio		BIT
		,Bit_SurtirArt			BIT
		,Bit_RegAut				BIT
		,Bit_SelPrecioMciaGral	BIT
		,Id_Num_Dpto			SMALLINT
	)

-- *********************************************************************  
-- ***** SELECT #9 venta de garantias - 2024-04-29
-- *********************************************************************  
	declare @cont int,
			@impWarranty money = 0;
	declare @tblnums as table
	( contador int
	);
	
	set @cont = 1;
	while @cont <= 1000
	begin
		insert into @tblnums select @cont;
		set @cont = @cont + 1;
	end

-- *****************************************************************
-- CHANGE.45622.Ajuste a información que OMS envía a PVS.
-- *****************************************************************
	declare @tblwarranties as table
	(
		Num_CodBarra_Art      bigint,
		Num_CodBarra_Garantia bigint,
		Importe_Garantia      money,
		contador              int
	);

	INSERT INTO @tblwarranties
	SELECT c.ProdItemBarcode      as Num_CodBarra_Art 
		  ,c.WarrantyBarcode      as Num_CodBarra_Garantia
		  ,cast((c.Price / (
				case 
					when c.ProdItemQuantity is null then 1 
					when c.ProdItemQuantity = 0 then 1 
					else c.ProdItemQuantity 
				end)) as money) as Importe_Garantia
		  ,d.contador
	FROM OrderFacts_OrderOptionProducts c
	JOIN @tblnums d ON c.ProdItemQuantity >= d.contador
	WHERE C.OrderNo=@Id_Num_Orden
	ORDER BY C.WarrantyBarcode, d.contador;

	select @impWarranty = SUM(a.Importe_Garantia) from @tblwarranties a;
	set @impWarranty = ISNULL(@impWarranty, 0);
-- *********************************************************************  
-- *********************************************************************  


-- *********************************************************************  
-- ***** SELECT #8 DE CUPONES MARKETEC - HU_21469_CONTABILIZACION - 2023-08-18
-- *********************************************************************  
	declare @tblMK as table
	(
		orderNo         int,
		productidSOT	int,
		barcodeSOT		bigint,

		cantidad        decimal(8,2),
		precioNormal    decimal(10,2),
		precioOferta    decimal(10,2),
		participacion   decimal(10,0),
		descripcion     varchar(175),
		ordenCupon      decimal(10,2),

		CouponCode      nvarchar(100),
		ValueByItem     decimal(10,2),
		ValueByOrder    decimal(10,2),
		nivel           smallint -- 1-orden, 2.articulo
	)

	if @ueType in ('CEDIS','DSV','DST')
	BEGIN
		delete @tblMK

		INSERT INTO @tblMK
		EXEC dbo.upCorpOms_Cns_OrderItemsByApportionMarketecWithCoupons @Id_Num_Orden;
	END

-- *************************************************************************************************************
-- BUG - 2023-05-16 - se añade prorrateo para surtido manual. - inicio
-- *************************************************************************************************************
	declare @tblprorrateo as table
	(
		id              int,
		orderNo         int,
		productidSOT	int,
		barcodeSOT		bigint,
		cantidad        decimal(8,2),

		precioNormal    decimal(10,2),
		precioOferta    decimal(10,2),
		participacion   decimal(10,0),
		descripcion     varchar(175),
		ordenTotal      decimal(10,2),
		ordenCupon      decimal(10,2),
		ordenTotCupon   decimal(10,2),
		PrecioAcum      decimal(10,2),
		artPromos       decimal(10,2),
		PrecioDDesc     decimal(10,2),
		artProrrateo    decimal(10,2),

		PosLineNumber   smallint,
		bitEncontrado   bit,
		DescripcionSustituto varchar(175),
		Id_Num_CodBarra_Sust numeric(14,0)
	)

	INSERT INTO @tblprorrateo
	EXEC dbo.upCorpOms_Cns_OrderItemsByApportion @Id_Num_Orden;

INSERT INTO		@T
 SELECT  
    CONVERT(DECIMAL,OrderFacts_POSProducts.PosBarcode)			AS Num_CodBarra  
   ,CONVERT(DECIMAL(12),OrderFacts_POSProducts.OrderNo)			AS Id_Num_Orden
   ,OrderFacts_POSProducts.PosProductDescription				AS Desc_Art
   ,OrderFacts_POSProducts.PosQuantity							AS Cant_Unidades  

	-- *****************************************************************************************************
	-- BUG - 2023-05-16 - se añade prorrateo para surtido manual.
	-- *****************************************************************************************************
   ,CASE WHEN @IsPickingManual = 1 THEN
		case when OrderFacts_POSProducts.PosBarcode = 2000020509772 then
			isnull(Round(OrderFacts_POSProducts.PosPriceNormalSale, 2, 1),0)
		else
			isnull(Round(getPOSPrice.precioNormal, 2, 1),0)
		end

	ELSE 
		-- BUG.49682
		case when isnull(Round(OrderFacts_POSProducts.PosPriceNormalSale, 2, 1),0) > 0 then 
			isnull(Round(OrderFacts_POSProducts.PosPriceNormalSale, 2, 1),0)
		else
			isnull(Round(Pinit.PosPriceNormalSale, 2, 1),0)
		end
    END AS Precio_VtaNormal  
    -- *****************************************************************************************************
	-- HU.12520.Prorrateo.de.cupones - 2023-03-05
	-- *****************************************************************************************************
	,CASE WHEN @IsPickingManual = 1 THEN
		case when OrderFacts_POSProducts.PosBarcode = 2000020509772 then
			isnull(Round(OrderFacts_POSProducts.PosPriceOfferSale, 2, 1),0)
		else
			isnull(Round(getPOSPrice.precioOferta, 2, 1),0)
		end
	ELSE 
		-- BUG.49682
		case when isnull(Round(OrderFacts_POSProducts.PosPriceOfferSale, 2, 1),0) > 0 then
			isnull(Round(OrderFacts_POSProducts.PosPriceOfferSale, 2, 1),0) -- + dbo.fn_GetAmmountCuponsByBarcode (@Id_Num_Orden, OrderFacts_POSProducts.PosBarcode)
		else
			isnull(Round(Pinit.PosPriceOfferSale, 2, 1),0)
		end
	END AS Precio_VtaOferta
	-- *****************************************************************************************************

   ,CASE WHEN @ueType IN ('SETC') THEN CONVERT(INT,OrderFacts_POSProducts.PosRounding)  
    ELSE CONVERT(INT,0)  
    END															AS Redondeo  
   ,CASE 
		WHEN CONVERT(DECIMAL,OrderFacts_POSProducts.PosBarcode) = 2000020509772 THEN CONVERT(BIT, 1)
		ELSE 
			CASE
				WHEN @ueType IN ('SETC')
					THEN OrderFacts_POSProducts.PosBitBestPrice
					ELSE CONVERT(BIT, 0)
			END
	END														AS Bit_SelMejorPrecio
   ,CASE WHEN @ueType IN ('SETC') THEN OrderFacts_POSProducts.PosBitProductSupplied     
    ELSE   
    CASE WHEN OrderFacts_POSProducts.PosBarcode IN (SELECT distinct BarCode FROM OrderFacts_CarrierCodes) THEN CONVERT(bit,0)  
    ELSE CONVERT(bit,1)  
    END  
    END															AS Bit_SurtirArt  
   ,CASE WHEN @ueType IN ('SETC') THEN OrderFacts_POSProducts.PosBitRecApproved      
    ELSE CONVERT(bit,1)  
    END															AS Bit_RegAut  
   ,CASE WHEN @ueType IN ('SETC') THEN OrderFacts_POSProducts.PosBitBestPrice      
    ELSE CONVERT(bit,0)  
    END															AS Bit_SelPrecioMciaGral  
   ,CASE WHEN @ueType IN ('SETC') THEN TYC_OrderProducts.Id_Num_Dpto
    ELSE CONVERT(bit,0)  
    END															AS Id_Num_Dpto  

 FROM dbo.OrderFacts_POSProducts (NOLOCK)

 LEFT JOIN dbo.TYC_OrderProducts
	ON		TYC_OrderProducts.CnscOrder = OrderFacts_POSProducts.CnscOrder
		AND	TYC_OrderProducts.OrderNo = OrderFacts_POSProducts.OrderNo
		AND	TYC_OrderProducts.BarCode = OrderFacts_POSProducts.PosBarcode
 LEFT JOIN dbo.OrderFacts_POSPromotions (NOLOCK)
	on	OrderFacts_POSProducts.CnscOrder   = OrderFacts_POSPromotions.CnscOrder   
	and OrderFacts_POSProducts.OrderNo    = OrderFacts_POSPromotions.OrderNo   
	and OrderFacts_POSProducts.PosBarcode = OrderFacts_POSPromotions.PosBarcode  

 LEFT JOIN dbo.OrderFacts_POSProdsInit (NOLOCK) as Pinit
	ON		Pinit.CnscOrder  = OrderFacts_POSProducts.CnscOrder
		AND	Pinit.OrderNo    = OrderFacts_POSProducts.OrderNo
		AND	Pinit.PosBarcode = OrderFacts_POSProducts.PosBarcode
	
 LEFT JOIN @tblprorrateo AS getPOSPrice
	on OrderFacts_POSProducts.OrderNo    = getPOSPrice.orderNo  
	and OrderFacts_POSProducts.PosBarcode = getPOSPrice.barcodeSOT


 WHERE OrderFacts_POSProducts.OrderNo = @Id_Num_Orden
 and NOT (OrderFacts_POSProducts.PosBarcode = '2000020509772' AND OrderFacts_POSProducts.PosPriceNormalSale=0);


-- 2023-11-16 - sumar los cupones marketec a prodcutos SETC
IF @ueType = 'SETC'
BEGIN
	update @T
	set Prec_Oferta= precioOferta + m.ValueByOrder
	from @T t
	join @tblMK m on m.orderNo=t.Id_Num_Orden and m.barcodeSOT=t.Id_Num_CodBarra and m.nivel=1;

	update @T
	set Prec_Oferta= precioOferta + m.ValueByItem
	from @T t
	join @tblMK m on m.orderNo=t.Id_Num_Orden and m.barcodeSOT=t.Id_Num_CodBarra and m.nivel=2;
END

-- *************************************************************************************************************
-- BUG - 2023-05-16 - se añade prorrateo para surtido manual. - FINAL
-- *************************************************************************************************************

--
-- Actualiza el Numero de departamento de los artículos que no lo tienen
--
	UPDATE		@T
			SET		Id_Num_Dpto = ISNULL(Art.Id_Num_Dpto,0)
		FROM	@T	T
		JOIN	Art
		ON		Art.Num_CodBarra = T.Id_Num_CodBarra
		WHERE	T.Id_Num_Dpto IS NULL

-- Si no se tiene Departamento se actualiza en 0
	UPDATE		@T
			SET		Id_Num_Dpto = ISNULL(Id_Num_Dpto,0)
		FROM	@T	T
		WHERE	T.Id_Num_Dpto IS NULL

-- Inicializa Bit_RegAut en 0 los articulos de los departamentos 18 y 245 (FyV)
	  UPDATE		@T
		SET		Bit_RegAut				= 0
		FROM	@T						T
		WHERE	Id_Num_Dpto			IN ( 18, 245 )

--
-- H.I. 20211202
-- Se actualiza la columna Bit_RegAut siempre y cuando la tienda a la cual pertenece la orden esté
-- configurada con el prametro 63 (TYC Cob Aut en PVS) en 1.
--
SELECT
		 @Id_Num_UN = StoreNum
		,@Bit_RegAut = CASE WHEN IsPickingManual = 1 THEN 0
							ELSE 1
						END
	FROM
		dbo.OrderFacts_UE (NOLOCK)
	WHERE
		OrderFacts_UE.OrderNo = @Id_Num_Orden

  SELECT
		@Ind_CobAut = CONVERT(TINYINT,ISNULL(UnPerm_Info,0))
	FROM
		UNPerm_Info
	WHERE
			Id_Num_UN = @Id_Num_UN
		AND	Id_Num_UnInfo = @Id_Num_UnInfo

  SELECT @Ind_CobAut = ISNULL(@Ind_CobAut,0)

--
-- H.I. 20211202
-- De acuerdo al parametro 64-'TYC Cob Aut en PVS FyV' de UNPermInfo de la tienda, si está en '1' se envían los 
-- artículos de F y V que se capturaron en TYC siempre y cuando también el parametro 63-'TYC Cob Aut en PVS'
-- tambien este en '1'
--
  SELECT
		@Ind_CobAutFyV = CONVERT(TINYINT,ISNULL(UnPerm_Info,0))
	FROM
		UNPerm_Info
	WHERE
			Id_Num_UN = @Id_Num_UN
		AND	Id_Num_UnInfo = @Id_Num_UnInfoFyV

  SELECT @Ind_CobAutFyV = ISNULL(@Ind_CobAutFyV,0)

  IF @Ind_CobAut = 1 -- La tienda a la cual pertenece la orden SÍ tiene el Cobro Automatico por TYC
	BEGIN
--
-- H.I. 20211202
-- De acuerdo al parametro 64-'TYC Cob Aut en PVS FyV' de UNPermInfo de la tienda, si está en '1' se envían los 
-- artículos de F y V que se capturaron en TYC siempre y cuando también el parametro 63-'TYC Cob Aut en PVS'
-- tambien este en '1'
--
	IF @Ind_CobAutFyV = 0 -- La tienda a la cual pertenece la orden NO tiene el Cobro Automatico por TYC para FyV y excluye los departamentos 18 y 245
	  UPDATE		@T
		SET		Bit_RegAut				= 1
		FROM	@T						T
		WHERE	@Bit_RegAut				= 1
		AND		NOT Id_Num_Dpto			IN ( 18, 245 )
	ELSE -- La tienda a la cual pertenece la orden SÍ tiene el Cobro Automatico por TYC para FyV
	  UPDATE		@T
		SET		Bit_RegAut				= 1
		FROM	@T						T
		WHERE	@Bit_RegAut				= 1
	END

-- HU.49521 - PAGO DE SERVICIOS
-- Si la orden es de Pago de Servicios, el RecordSet de artículos debe ir vacio.
if @ueType = 'PSER'
  DELETE @T

--SELECT @Bit_RegAut AS Bit_RegAut
--SELECT @Ind_CobAut AS Ind_CobAut
--SELECT @Ind_CobAutFyV AS Ind_CobAutFyV
--
-- RecordSet detalle de articulos
--

SELECT
		 CONVERT(DECIMAL,Id_Num_CodBarra)	AS Num_CodBarra
		,CONVERT(DECIMAL(12),Id_Num_Orden)	AS Id_Num_Orden
		,CONVERT(VARCHAR(175),Desc_Art)		AS Desc_Art
		,CONVERT(DECIMAL(9,3),Cant_Surtida)	AS Cant_Unidades
		,Prec_Normal						AS Precio_VtaNormal
		,Prec_Oferta						AS Precio_VtaOferta
		,CONVERT(INT,Bit_Redondeo)			AS Redondeo
		,Bit_SelMejorPrecio					AS Bit_SelMejorPrecio
		,Bit_SurtirArt						AS Bit_SurtirArt
		,Bit_RegAut							AS Bit_RegAut
		,Bit_SelPrecioMciaGral				AS Bit_SelPrecioMciaGral  
		--,Id_Num_Dpto						AS Id_Num_Dpto
	FROM
		@T
 
-- *********************************************************************  
-- *********************************************************************  
  
-- *********************************************************************  
-- 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2  
-- Segundo Record Set. Forma de Pago.  
-- 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2 - 2  
-- *********************************************************************  
  
DECLARE @tipoUE char(4) = null;  
DECLARE @Imp_Pago Money = null;
DECLARE	@Imp_Pago1 Decimal(12,2) = null;
  
SET @tipoUE = ISNULL(  
(SELECT 'SFCC' FROM OrderFacts_UE UE  
WHERE UE.OrderNo = @Id_Num_Orden  
and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
and isnumeric(UE.CreatedBy) = 0  
--and (UE.DeliveryType LIKE '%domicilio%' OR UE.DeliveryType LIKE '%homeDelivery%')  
)  
-- 2021/09/30 -- pedidos telefonicos - ini
, CASE WHEN (SELECT UE.CreatedBy FROM OrderFacts_UE UE  
  WHERE UE.OrderNo = @Id_Num_Orden  
               and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')) = '27' 
			   THEN 'SFCC' 
			   ELSE 'ECOM' 
  END);  
-- 2021/09/30 -- pedidos telefonicos - fin
  
--SELECT @tipoUE;  
  
-- 2022/02/26 -- Obtener importe total de la orden - INICIO
	--SELECT	@Imp_Pago =  PaymentMethodAmount
	--	FROM		OrderFacts_POSMethodPayment
	--	WHERE		OrderNo		= @Id_Num_Orden

	--SET		@Imp_Pago	= ISNULL( @Imp_Pago , 0 )
IF @ueType in ('SETC','PSER')
  BEGIN
	SELECT	top (1) -- 2023-11-02 tomar la 1er forma de pago para ordenes SORIANA 
			@Imp_Pago =  PaymentMethodAmount
		FROM		OrderFacts_POSMethodPayment
		WHERE		OrderNo		= @Id_Num_Orden

	SET		@Imp_Pago	= ISNULL( @Imp_Pago , 0 )
  END
ELSE
  BEGIN
	SELECT
		@Imp_Pago = SUM(CONVERT(DECIMAL(9,3),Cant_Surtida) * ISNULL(Prec_Oferta,Prec_Normal))
	FROM
		@T
  END
-- 2022/02/26 -- Obtener importe total de la orden - FIN
--
--	DATOS DEL CUPÓN
--
DECLARE		 @cuponPrice				MONEY
			,@cuponPriceAdjustmentId	VARCHAR(100)
			,@cuponPromotionId			VARCHAR(255)

--	2022/03/21	AGR
--	SE ELIMINA LA NOTIFICACIÓN DEL CUPÓN A NIVEL CABECERA PARA SER REMPLAZADO
--	POR UNA DISTRIBUCIÓN POR ARTÍCULO (VER LA FUNCIÓN getPOSItemCouponDistribution)
--
IF ( 1 = 0 )
	SELECT		 @cuponPrice				= ABS( A.price )
				,@cuponPriceAdjustmentId	= A.priceAdjustmentId
				,@cuponPromotionId			= A.promotionId
		FROM	OrderFacts_UE_PriceAdjOrder A
		WHERE		A.OrderNo						= @Id_Num_Orden
	-- 2022/02/26 -- El cupón solo se manda para órdenes de SETC - INICIO
				AND A.[custom] = 0
				AND	@ueType						= 'SETC'
	-- 2022/02/26 -- El cupón solo se manda para órdenes de SETC - FIN

------------------------------------------------------------------------
--	REDONDEO Y DONACIÓN PARA FUNDACIÓN SORIANA
------------------------------------------------------------------------
DECLARE
		 @Bit_Redondeo		BIT
		,@Imp_Donacion		MONEY

SET @Bit_Redondeo = 0
SET @Imp_Donacion = 0

SELECT @Bit_Redondeo = SorianaDonation
FROM OrderFacts_JsonExtension
WHERE OrderNo = @Id_Num_Orden
	
--SET	  @Imp_Donacion =  case (@Bit_Redondeo)
--						when 0 then 0
--						when 1 then 5.50
--						END

-- *********************************************************************  
-- ***** SELECT #2 DE SALIDA - Datos del metodo de pago  
-- *********************************************************************  
-- extraer y formar la tabla prorrateada de las participaciones por forma de pago de la orden.
	declare @bitAviso bit =0,
			@conteoMP int = 0,
			@importePedido decimal(18,3),
			@pedido bigint,
			@jsonMessage varchar(max),
			@firstOrder int;

	DECLARE @count INT,
			@index INT = 0, 
			@id INT,
			@payment_method_id varchar (100),
			@amount varchar (100);

	declare @tempSF as table
	(
		jid int identity(1,1),
		pedido bigint,
		payment_method_id varchar (100),
		amount decimal(18,3),
		participacion decimal(18,3),
		importecalc decimal(18,3),
		correctivo  decimal(18,3)
	);

	declare @importes as table
	(	ordernoPos    bigint,
		orderno		  int,
		importe	      decimal(18,2),
		participacion decimal(18,2),
		monto         decimal(18,2)
	);

	declare @tempOMS as table
	(
		jid                         int identity(1,1),
		pedido                      bigint,
		orderNo                     int,
		payment_method_id           varchar (100),
		amount                      decimal(18,2),
		participacion               decimal(18,4),

		PaymentMethodId             smallint,
		PaymentMethodDesc           varchar(40),
		PaymentMethodAccount        varchar(40),
		PaymentMethodAmount         money,
		PaymentMethodExpDate        varchar(10),
		PaymentMethodBitEncrypted   bit,
		PaymentMethodEncryptAccount varchar(44), 
		PaymentMethodEncryptExpDate varchar(24),
		PaymentMethodIdKey          varchar(40),
		UeNo                        varchar(25)
	);

	declare @tblprorrateov2 as table
	(
		id              int,
		orderNo         int,
		productidSOT	int,
		barcodeSOT		bigint,
		cantidad        decimal(8,2),

		precioNormal    decimal(10,2),
		precioOferta    decimal(10,2),
		participacion   decimal(10,0),
		descripcion     varchar(175),
		ordenTotal      decimal(10,2),
		ordenCupon      decimal(10,2),
		ordenTotCupon   decimal(10,2),
		PrecioAcum      decimal(10,2),
		artPromos       decimal(10,2),
		PrecioDDesc     decimal(10,2),
		artProrrateo    decimal(10,2),

		PosLineNumber   smallint,
		bitEncontrado   bit,
		DescripcionSustituto varchar(175),
		Id_Num_CodBarra_Sust numeric(14,0)
	)

	INSERT INTO @tblprorrateov2
	EXEC dbo.upCorpOms_Cns_OrderItemsByApportionFull @Id_Num_Orden;

	select top (1) @conteoMP = COUNT(*) from OrderFacts_POSMethodPayment a where a.OrderNo = @Id_Num_Orden;
	if @conteoMP > 1 
		set @bitAviso =1;
	set @bitAviso =1; -- siempre encendido

	-- solo aplica en ordenes con mas de 1 metodo de pago
	if @bitAviso = 1
		begin
			-- obtener el pedido de SF
			select top (1) @pedido = x.OrderNoPOS
			from OrderFacts_UE_Transition x
			where x.OrderNo = @Id_Num_Orden;
			
			-- obtener las consignaciones y total de cada una de ellas
			insert into @importes
			select y.OrderNoPOS, x.OrderNo, 0, 0, 0
			from OrderFacts_UE x
			join OrderFacts_UE_Transition y on y.OrderNo =  x.OrderNo
			where y.OrderNoPOS = @pedido;

			-- valores de los productos en orderfacts_posProducts, contienen el descuento marketec inclusive
			update @importes
				set importe = yy.importe
			from @importes x
			join 
				(select distinct OrderNo, SUM(case when y.PosQuantity > 0 then (y.PosPriceOfferSale * y.posquantity) else (y.PosPriceNormalSale * y.posquantity) end) importe 
					from OrderFacts_POSProdsInit y
					group by y.OrderNo) yy 
				on yy.OrderNo = x.orderno;

			-- valores de los productos en OrderFacts_OrderOptionProducts, se suman las garantias si es necesario
			update @importes
				--set importe = x.importe + yy.importe,
				set monto = x.monto + yy.importe
			from @importes x
			join 
				(select distinct OrderNo, SUM(y.Price * y.ProdItemQuantity) importe 
					from OrderFacts_OrderOptionProducts y
					group by y.OrderNo) yy 
				on yy.OrderNo = x.orderno;

			-- participacion prorrateada por consignacion
			if @ueType <> 'PSER'
				begin
					update @importes
						set participacion = y.participacion,
						monto = x.monto + y.pofer
					from @importes x
					join (select y1.orderNo, SUM(y1.participacion) participacion, SUM(y1.cantidad * y1.precioOferta) pofer, SUM(y1.artProrrateo) prorr from @tblprorrateov2 y1 group by y1.orderNo) y
						on y.orderNo = x.orderno
				end
			else
				begin
					update @importes
						set participacion = y.participacion,
						monto = x.monto + y.pofer
					from @importes x
					join (select y1.orderNo, SUM(y1.participacion) participacion, SUM(y1.precioOferta * y1.cantidad) pofer, SUM(y1.artProrrateo) prorr from @tblprorrateov2 y1 group by y1.orderNo) y
						on y.orderNo = x.orderno
-- 2024-12-18 - HU.49521 Inicio
					--
					-- Agrega el imoprte de la comision al total de la forma de pago.
					--
					update @importes
						set  importe = x.importe + y.ImporteCmsn
							,monto = x.monto + y.ImporteCmsn
					from @importes x
					join OrderFacts_ServiciosItems y
						on y.orderNo = x.orderno
-- 2024-12-18 - HU.49521 Fin
				end

			-- agregar el flete del envio por consignacion
			update @importes
				set monto = x.monto + y.monto
			from @importes x
			join (select y1.orderNo, SUM(y1.PosQuantity * y1.PosPriceOfferSale) monto from OrderFacts_POSProducts y1 where y1.PosBarcode = 2000020509772 group by y1.orderNo) y
				on y.orderNo = x.orderno;
			
			update @importes set importe = monto;

			-- obtener importe total del pedido
			select @importePedido = SUM(x.importe) from @importes x;

			-- extraer json del pedido
			select top (1) @jsonMessage = x.SF_Json from tbl_Descarga_Order_SF x where x.order_no = format(@pedido, '0000000000');
	
			-- extraer los distintos formas de pago del pedido y se importe
			set @index =0;
			SELECT @count = COUNT(*)
			FROM OPENJSON(@jsonMessage,'$.data.payment_instruments');

			WHILE(@index < @count)
				BEGIN
					SET @id = @index + 1 ;
					SET @payment_method_id = CONCAT('$.data.payment_instruments[', @index ,'].payment_method_id');
					SET @payment_method_id = (JSON_VALUE(@jsonMessage, @payment_method_id));

					SET @amount = CONCAT('$.data.payment_instruments[', @index ,'].amount');
					SET @amount = (JSON_VALUE(@jsonMessage, @amount));

					insert into @tempSF 
					SELECT @pedido, 
						@payment_method_id,
						case when isnumeric(@amount) = 1 then CONVERT(decimal(18,3), @amount) else 0 end,
						0,0,0;

					SET @index = @index + 1;
				END

			-- participacion de cada forma de pago.
--			update @tempSF set participacion = (amount * 100) / @importePedido;
			if @conteoMP > 1 
				update @tempSF set participacion = (amount * 100) / @importePedido;
			else
				update @tempSF set participacion = (@importePedido * 100) / @importePedido;

			update @tempSF set payment_method_id = case when payment_method_id='CREDIT_CARD' then 'CardType' else payment_method_id end

			-- formas de pago por cada consignacion
			insert into @tempOMS
			select 
				y.OrderNoPOS,
				x.OrderNo,
				x.PaymentMethodDesc,
				0,
				0,
				x.PaymentMethodId,
				x.PaymentMethodDesc,
				x.PaymentMethodAccount,
				x.PaymentMethodAmount,
				x.PaymentMethodExpDate,
				x.PaymentMethodBitEncrypted,
				x.PaymentMethodEncryptAccount, 
				x.PaymentMethodEncryptExpDate,
				x.PaymentMethodIdKey,
				x.UeNo
			from OrderFacts_POSMethodPayment x
			join OrderFacts_UE_Transition y on y.OrderNo = x.OrderNo
			where y.OrderNoPOS = @pedido;

			-- participacion de cada forma de pago en las consignación
			update @tempOMS 
				set participacion = y.participacion
			from @tempOMS x
			join @tempSF y 
				on y.pedido = x.pedido
				and y.payment_method_id = x.payment_method_id;
			
			-- redondeo de participacion por consignacion
			update @tempOMS
				set participacion = x1.participacion + X3.correctivo
			from @tempOMS x1
			join (	select	x.orderNo, 
					x.payment_method_id,
					ROW_NUMBER() OVER(PARTITION BY x.orderNo ORDER BY x.orderNo, x.payment_method_id) fila
					from @tempOMS x) x2
				on x2.orderNo=x1.orderNo 
				and x2.payment_method_id = x1.payment_method_id
				and x2.fila=1
			join (	select	x.orderNo, 
					case when SUM(x.participacion) < 100 then (100 - SUM(x.participacion)) else 0 end correctivo
					from @tempOMS x
					group by x.orderNo ) X3
				on X3.orderNo = x1.orderNo;

			-- importe por cada forma de pago y consignación
			update @tempOMS 
				set amount = (y.importe * x.participacion) / 100
			from @tempOMS x
			join @importes y 
				on y.ordernoPos = x.pedido
				and y.orderno = x.orderNo;
		end

-- tabla de metodos de pago por la consignacion
declare @record2 as table
(
	Id_Num_FormaPago        smallint,
	Id_Num_FormaPagoVersion smallint,
	Desc_FormaPago          VARCHAR(40),
	Cuenta                  VARCHAR(10),
	Imp_Pago                money,
	Fec_Venc                VARCHAR(10),
	Bit_Cifrado             bit,
	Cuenta_enc              VARCHAR(44),
	Vec_enc                 VARCHAR(24),
	IdKey                   VARCHAR(40),
	Referencia              VARCHAR(20),
	CuponPrice              MONEY,
	CuponPriceAdjustmentId  VARCHAR(100),
	CuponPromotionId        VARCHAR(255),
	Bit_Redondeo            bit,
	Imp_Donacion            money,
	consecutivo             int identity (1,1) -- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO
)

INSERT INTO @record2
SELECT  
		-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO
		-- TOP (1) -- 2023-11-02 tomar la 1er forma de pago para ordenes SORIANA
		CASE WHEN @tipoUE = 'ECOM' THEN opm.PaymentMethodId  
			ELSE   
				CASE 
					WHEN @ueType = 'SETC' THEN 
						CASE 
							-- 2023-10-13 - HU_14161 - valdaciones para ordenes desde app_ceres
							--WHEN @CreatedBy <> '27' THEN
							WHEN not (@CreatedBy IN ('27','app_ceres','app_picking')) THEN
								CASE	WHEN opm.PaymentMethodDesc <> 'PayPal'
									THEN om.Id_Num_FormaPago
									ELSE CONVERT(SMALLINT,1)
								END
							ELSE  opm.PaymentMethodId
						END
					ELSE CONVERT(SMALLINT,25)
				END  
		END									AS Id_Num_FormaPago  
		,CONVERT(SMALLINT,2)				AS Id_Num_FormaPagoVersion
		,CASE 
			WHEN @tipoUE = 'ECOM' THEN opm.PaymentMethodDesc  
			ELSE   
				CASE 
					-- 2023-10-13 - HU_14161 - valdaciones para ordenes desde app_ceres
					--WHEN @ueType = 'SETC' and @CreatedBy <> '27' THEN
					WHEN @ueType = 'SETC' and not (@CreatedBy IN ('27','app_ceres','app_picking')) THEN
						CASE 
							WHEN opm.PaymentMethodDesc <> 'PayPal'
							THEN om.Desc_FormaPago
							ELSE CONVERT(VARCHAR(40),'Tarjeta de crédito')
						END
					ELSE CONVERT(VARCHAR(40),'Procesador de Pago Soriana')
				END			
		END													AS Desc_FormaPago  
		,'99999999'											AS Cuenta  
		-- ,@Imp_Pago										AS Imp_Pago 
		--,opm.PaymentMethodAmount                          AS Imp_Pago
		,opm.amount                                         AS Imp_Pago
		,opm.PaymentMethodExpDate							AS Fec_Venc  
		,CASE opm.PaymentMethodBitEncrypted  
			WHEN 1 THEN 1  
			ELSE 0  
		END													AS Bit_Cifrado  
		,ISNULL( opm.PaymentMethodEncryptAccount, '' )		AS Cuenta_enc  
		,ISNULL( opm.PaymentMethodEncryptExpDate, '' )		AS Vec_enc  
		,ISNULL( opm.PaymentMethodIdKey, '' )				AS IdKey  
		,CASE 
			WHEN   
				(CASE WHEN @tipoUE = 'ECOM' THEN opm.PaymentMethodId  
					ELSE   
					CASE WHEN @ueType = 'SETC' THEN om.Id_Num_FormaPago  
					ELSE 1  
					END  
				END) = 1  
			THEN CASE WHEN opm.PaymentMethodAccount = '' 
					THEN '999999******9999'
					ELSE opm.PaymentMethodAccount
					END
			ELSE
					CASE WHEN opm.PaymentMethodDesc <> 'PayPal'
					THEN ''
					ELSE CONVERT(VARCHAR(40),@Id_Num_Orden16)
					END  
		END												AS Referencia  
		,ISNULL( @cuponPrice , 0 )						AS CuponPrice
		,ISNULL( @cuponPriceAdjustmentId , '' )			AS CuponPriceAdjustmentId
		,ISNULL( @cuponPromotionId , '' )				AS CuponPromotionId
		,@Bit_Redondeo									AS Bit_Redondeo
		,@Imp_Donacion									AS Imp_Donacion
--	FROM		dbo.OrderFacts_POSMethodPayment (NOLOCK)	opm  
	FROM		@tempOMS opm  
	LEFT JOIN	dbo.OrderFacts_methodPayment				om 
	ON			om.PaymentMethodDesc	= opm.PaymentMethodDesc  
	WHERE		OrderNo					= @Id_Num_Orden 
	ORDER BY om.PaymentMethodDesc

-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO
declare @primero int;
select top (1) @primero = a.consecutivo from @record2 a order by a.consecutivo;
-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO

declare @imp_mkt money;

select @imp_mkt = SUM(abs(m1.ValueByOrder)) from @tblMK m1 where m1.nivel=1;
select @imp_mkt = @imp_mkt + SUM(abs(m2.ValueByItem)) from @tblMK m2 where m2.nivel=2;
SET @imp_mkt = ISNULL(@imp_mkt,0);

declare @imp_pser money;
select top (1) @imp_pser = x.PaymentMethodAmount from @tempOMS x;

set @Imp_Pago = 0;
select @Imp_Pago = SUM(a.Imp_Pago) from @record2 a;
-- if @ueType = 'PSER' 
--    set @Imp_Pago = @imp_pser;

select 
	 --MOSTRAR SOLO UNA FORMA DE PAGO CON EL TOTAL
	top (1)
	Id_Num_FormaPago,
	Id_Num_FormaPagoVersion,
	Desc_FormaPago,
	Cuenta,
	 --HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO
	(@Imp_Pago - @imp_mkt) [Imp_Pago],
	Fec_Venc,
	Bit_Cifrado,
	Cuenta_enc,
	Vec_enc,
	IdKey,
	Referencia,
	CuponPrice,
	CuponPriceAdjustmentId,
	CuponPromotionId,
	Bit_Redondeo,
	Imp_Donacion
from @record2;


--select 
--	-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO
--	-- top (1)
--	Id_Num_FormaPago,
--	Id_Num_FormaPagoVersion,
--	Desc_FormaPago,
--	Cuenta,
--	-- change.45622.Venta de Garantías. Ajuste a información que OMS envía a PVS.
--	-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO
--	case 
--		when consecutivo = @primero then (Imp_Pago - @imp_mkt) 
--		else Imp_Pago
--	end [Imp_Pago],
--	Fec_Venc,
--	Bit_Cifrado,
--	Cuenta_enc,
--	Vec_enc,
--	IdKey,
--	Referencia,
--	CuponPrice,
--	CuponPriceAdjustmentId,
--	CuponPromotionId,
--	Bit_Redondeo,
--	Imp_Donacion
--from @record2;


-- ********************************************************************* 
-- 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3  
-- Tercer Record Set. Datos del Cliente.  
-- Obtiene el Total de Puntos  
-- 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3 - 3  
-- *********************************************************************  
  
  
-- *********************************************************************  
-- ***** SELECT #3 DE SALIDA - Datos del cliente  
-- *********************************************************************  
-- 2024-04-29 - HU.40510 - Ventas garantías MG
-- Obtener el telefono del cliente
-- Prioridades:
-- 1. el celular de contacto en OrderFacts_JsonExtension
-- 2. el telefono del cliente en OrderFacts_JsonExtension
-- 3. el telefono de la direccion de envio en OrderFacts_ue
-- 4. el telefono de la dirección de facturación en OrderFacts_Order
   declare @telefono varchar(40) = '';
   select top (1) @telefono = 
		CASE 
		WHEN NOT (json_value( replace(replace(replace(x.ExtensionData,'[',''),']',''),'''','"'), '$.phone_mobile') IS NULL)
			THEN json_value( replace(replace(replace(x.ExtensionData,'[',''),']',''),'''','"'), '$.phone_mobile')
		ELSE 
			CASE 
			WHEN NOT (json_value( replace(replace(replace(x.ExtensionData,'[',''),']',''),'''','"'), '$.phone_home') IS NULL)
				THEN json_value( replace(replace(replace(x.ExtensionData,'[',''),']',''),'''','"'), '$.phone_home')
			ELSE 
				CASE 
				WHEN LEN(RTRIM(LTRIM(y.Phone))) > 0
				THEN y.Phone
				ELSE
					ISNULL(yy.Phone,'')
				END
			END
		END
   from OrderFacts_JsonExtension x
   join OrderFacts_UE y on y.OrderNo = x.OrderNo
   join OrderFacts_Order yy on yy.OrderNo = x.OrderNo
   where x.OrderNo = @Id_Num_Orden;

  SELECT  
   PC.CustomerNo      AS Id_Num_Cte  
  ,CASE   
   WHEN @tipoUE = 'ECOM' THEN ISNULL(PC.CustomerFirstName,'')  
   ELSE ISNULL(O.CustomerName,'')  
   END        AS Nom_Cte  
  ,isnull(PC.CustomerLastname1,'') AS Ap_Paterno  
  ,isnull(PC.CustomerLastname2,'') AS Ap_Materno  
    
  ,CASE   
 --WHEN @tipoUE = 'SFCC' THEN '2000100011942798'    -- EXCEPCION PARA UAT - 2021-08-08  
   WHEN RTRIM(LTRIM(PC.CustomerPersonalId)) = '0' THEN NULL  
   ELSE RTRIM(LTRIM(PC.CustomerPersonalId))  
   END  
           AS Cve_IdCte -- Numero de tarjeta Bonomatico o Del Aprecio.  
  ,CASE   
   --WHEN @tipoUE = 'SFCC' THEN 900        -- EXCEPCION PARA UAT - 2021-08-08  
   WHEN PC.CustomerTotalPoints IS null then 0  
   ELSE ISNULL(PC.CustomerTotalPoints,0)   
   END        AS TotalPuntos  
   ,@telefono AS Telefono
 FROM  
  dbo.OrderFacts_POSClient (NOLOCK) AS PC  
  LEFT JOIN dbo.OrderFacts_Order (NOLOCK) AS O ON O.CnscOrder = PC.CnscOrder AND O.OrderNo = PC.OrderNo  
 WHERE PC.OrderNo = @Id_Num_Orden  
-- *********************************************************************  
-- *********************************************************************  
     
  
-- *********************************************************************  
-- 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4  
-- Cuarto Record Set. Detalle de la Orden de Articulos con Puntos.  
-- 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4 - 4  
-- *********************************************************************  
  
  
-- *********************************************************************  
-- ***** SELECT #4 DE SALIDA - PROMOCIONES  
-- *********************************************************************  
IF @tipoUE = 'ECOM'  
BEGIN  
  SELECT  
   CONVERT(DECIMAL,PosBarcode)   AS Num_CodBarra  
  ,OrderNo        AS Id_Num_Orden  
  ,isnull(PosAdditionalPoints,0)   AS Puntos_Adic  
  ,isnull(PosAccumPointsExch,0)   AS CantUni_Canje -- cantidad de piezas canjeadas  
  ,isnull(PosRedemptionPoints,0)   AS Puntos_Canje -- puntos canjeados  
  ,isnull(PosAmountExch,0)    AS Precio_Canje  
  ,isnull(PosPercentApplied,0)   AS Porc_Desc  
 FROM  
  dbo.OrderFacts_POSPromotions (NOLOCK) AS InfoPromocion    
 WHERE OrderNo = @Id_Num_Orden  
 AND NOT (PosBarcode = 0  
 )  
END  
ELSE  
BEGIN  

  SELECT	 Num_CodBarra
			,Id_Num_Orden
			,Puntos_Adic
			,CantUni_Canje
			,Puntos_Canje
			,Precio_Canje
			,Porc_Desc	
	FROM	dbo.getPOSPromotions_V03( @Id_Num_Orden )
  
END  
-- *********************************************************************  
-- *********************************************************************  
  

-- *********************************************************************  
-- ***** SELECT #5 DE SALIDA - Formas de pago  
-- *********************************************************************  
 DECLARE @Promotions NVARCHAR(MAX) = NULL,  
   @OrderPOS INT = NULL;  
  
 SET @OrderPOS = ISNULL((SELECT TOP 1 OrderNoPOS FROM OrderFacts_UE_Transition WHERE OrderNo=@Id_Num_Orden), 0);  
  
 IF OBJECT_ID('tempdb.dbo.#mensaje', 'U') IS NOT NULL  
   DROP TABLE #mensaje;   
 IF OBJECT_ID('tempdb.dbo.#formas', 'U') IS NOT NULL  
   DROP TABLE #formas;   
  
 CREATE TABLE #mensaje  
  (  
   OrderNo int,  
   OrderJSON nvarchar(max),  
   c_loyaltyPromotions nvarchar(max)  
  )  
  
 CREATE TABLE #formas  
  (  
   OrderNo int, 
   FormaPago int,  
   NombrePOS varchar(50),  
   NombreSFCC varchar(50),  
   Valor decimal(18,2),  
   Referencia varchar(50),
   Bit_Cred bit,
   consecutivo int identity(1,1) -- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO	 
  )  
  
 IF @tipoUE = 'SFCC'  
	  BEGIN  
		   INSERT INTO #formas  
		   select   
			 -- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO	 
			 -- TOP (1) -- 2023-11-02 tomar la 1er forma de pago para ordenes SORIANA
			 t.OrderNo, --t.OrderNoPOS,  
			 CASE WHEN ISNULL(  
			  (SELECT 'SFCC' FROM OrderFacts_UE UE  
			  WHERE UE.OrderNo = opm.OrderNo  
			  and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
			  and isnumeric(UE.CreatedBy) = 0), 
			  -- 2021/09/30 -- pedidos telefonicos - ini
			  CASE WHEN (SELECT UE.CreatedBy FROM OrderFacts_UE UE  
					 WHERE UE.OrderNo = @Id_Num_Orden  
					   and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')) = '27' 
					   THEN 'SFCC' 
					   ELSE 'ECOM' 
			  END)
			  -- 2021/09/30 -- pedidos telefonicos - fin
			  = 'ECOM' THEN opm.PaymentMethodId  
			  ELSE   
				  CASE WHEN opm.PaymentMethodDesc <> 'PayPal'
					THEN --isnull(om.Id_Num_FormaPago,opm.PaymentMethodId)
					  CASE WHEN opm.PaymentMethodDesc <> 'PIS'
						THEN isnull(om.Id_Num_FormaPago,opm.PaymentMethodId)
						ELSE CONVERT(SMALLINT,42) --Procesador de Pago Soriana - PIS. bug.51168
					  END
					ELSE CONVERT(SMALLINT,1)
				  END
			 END  
			,CASE WHEN ISNULL(  
			  (SELECT 'SFCC' FROM OrderFacts_UE UE  
			  WHERE UE.OrderNo = opm.OrderNo  
			  and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
			  and isnumeric(UE.CreatedBy) = 0), 
			  -- 2021/09/30 -- pedidos telefonicos - ini
			  CASE WHEN (SELECT UE.CreatedBy FROM OrderFacts_UE UE  
					 WHERE UE.OrderNo = @Id_Num_Orden  
					   and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')) = '27' 
					   THEN 'SFCC' 
					   ELSE 'ECOM' 
			  END)  
			  -- 2021/09/30 -- pedidos telefonicos - fin
			  = 'ECOM' THEN opm.PaymentMethodDesc  
			  ELSE --isnull(om.Desc_FormaPago,opm.PaymentMethodDesc) 
				  CASE WHEN opm.PaymentMethodDesc <> 'PIS'
					THEN isnull(om.Desc_FormaPago,opm.PaymentMethodDesc)
					ELSE opm.PaymentMethodDesc
				  END
			 END  
			,CASE WHEN ISNULL(  
			  (SELECT 'SFCC' FROM OrderFacts_UE UE  
			  WHERE UE.OrderNo = opm.OrderNo  
			  and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
			  and isnumeric(UE.CreatedBy) = 0), 
			  -- 2021/09/30 -- pedidos telefonicos - ini
			  CASE WHEN (SELECT UE.CreatedBy FROM OrderFacts_UE UE  
					 WHERE UE.OrderNo = @Id_Num_Orden  
					   and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')) = '27' 
					   THEN 'SFCC' 
					   ELSE 'ECOM' 
			  END)  
			  -- 2021/09/30 -- pedidos telefonicos - fin
			  = 'ECOM' THEN opm.PaymentMethodDesc  
			  ELSE --isnull(om.Desc_FormaPago,opm.PaymentMethodDesc)  
				  CASE WHEN opm.PaymentMethodDesc <> 'PIS'
					THEN isnull(om.Desc_FormaPago,opm.PaymentMethodDesc)
					ELSE opm.PaymentMethodDesc
				  END
			 END  
			--,opm.PaymentMethodAmount  
			,opm.amount
			--    ,opm.PaymentMethodAccount  
			,CASE WHEN opm.PaymentMethodAccount = '' 
					THEN 
					  CASE WHEN opm.PaymentMethodDesc <> 'PayPal'
						THEN '999999******9999'
						ELSE CONVERT(VARCHAR(40),@Id_Num_Orden16)
					  END  
					ELSE opm.PaymentMethodAccount
				 END
			,NULL
		   -- FROM dbo.OrderFacts_POSMethodPayment (NOLOCK) opm  
		   FROM @tempOMS opm  
		   LEFT JOIN dbo.OrderFacts_methodPayment om on om.PaymentMethodDesc = opm.PaymentMethodDesc  
		   left join dbo.OrderFacts_UE_Transition t on t.OrderNo = opm.OrderNo and t.UeNo=opm.UeNo
		   --WHERE t.OrderNoPOS in (select f.OrderNo from #formas f);  
		   WHERE t.OrderNo in (@Id_Num_Orden)
		   and opm.OrderNo in (@Id_Num_Orden)
		   ORDER BY opm.PaymentMethodDesc
	  END   
 ELSE  
	  BEGIN  
		   INSERT INTO #formas  
		   select   
			TOP (1) -- 2023-11-02 tomar la 1er forma de pago para ordenes SORIANA
			t.OrderNoPOS,  
			CASE WHEN ISNULL(  
			  (SELECT 'SFCC' FROM OrderFacts_UE UE  
			  WHERE UE.OrderNo = opm.OrderNo  
			  and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
			  and isnumeric(UE.CreatedBy) = 0), 'ECOM')  
			  = 'ECOM' THEN opm.PaymentMethodId  
			  ELSE   
				  CASE WHEN opm.PaymentMethodDesc <> 'PayPal'
					THEN isnull(om.Id_Num_FormaPago,opm.PaymentMethodId)
					ELSE CONVERT(SMALLINT,1)
				  END
			 END  
			,CASE WHEN ISNULL(  
			  (SELECT 'SFCC' FROM OrderFacts_UE UE  
			  WHERE UE.OrderNo = opm.OrderNo  
			  and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
			  and isnumeric(UE.CreatedBy) = 0), 'ECOM')  
			  = 'ECOM' THEN opm.PaymentMethodDesc  
			  ELSE isnull(om.Desc_FormaPago,opm.PaymentMethodDesc)  
			 END  
			,CASE WHEN ISNULL(  
			  (SELECT 'SFCC' FROM OrderFacts_UE UE  
			  WHERE UE.OrderNo = opm.OrderNo  
			  and UE.UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')  
			  and isnumeric(UE.CreatedBy) = 0), 'ECOM')  
			  = 'ECOM' THEN opm.PaymentMethodDesc  
			  ELSE isnull(om.Desc_FormaPago,opm.PaymentMethodDesc)  
			 END  
			--,opm.PaymentMethodAmount  
			,opm.amount
			--    ,opm.PaymentMethodAccount  
			,CASE WHEN opm.PaymentMethodAccount = '' 
					THEN 
					  CASE WHEN opm.PaymentMethodDesc <> 'PayPal'
						THEN '999999******9999'
						ELSE CONVERT(VARCHAR(40),@Id_Num_Orden16)
					  END  
					ELSE opm.PaymentMethodAccount
				 END
			,NULL
		   --FROM dbo.OrderFacts_POSMethodPayment (NOLOCK) opm  
		   FROM @tempOMS opm  
		   LEFT JOIN dbo.OrderFacts_methodPayment om on om.PaymentMethodDesc = opm.PaymentMethodDesc  
		   left join dbo.OrderFacts_UE_Transition t on t.OrderNo = opm.OrderNo  
		   WHERE opm.OrderNo = @Id_Num_Orden;  
	  END  
--
-- Obtiene si la tarjeta es de Credito o de Debito en la tabla #formas
--
UPDATE
		#formas
	SET
		Bit_Cred = pdpBin.Bit_Cred
	FROM
		#formas
	JOIN
		pdpBin
	ON
			pdpBin.Id_Cve_Bin = LEFT(#formas.Referencia,6)
	JOIN
		pdpBinProd
	ON
			pdpBinProd.Id_Cve_Bin = pdpBin.Id_Cve_Bin

-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO	 
 --IF @UeType in ('SETC','DST','CEDIS','DSV','SERVICIOS','RPM','PSER')
 --  BEGIN
 --    UPDATE
	--		#formas
	--	SET
	--		Valor = @Imp_Pago
	--	WHERE
	--		FormaPago IN ( 1, 5 ) -- TdeC y Efvo
 --  END

--
-- Actualización de Forma de Pago Omonel
--
  UPDATE
		#formas
	SET
		 FormaPago = 12
		,NombrePOS = pdpBinProd.Desc_Marca
		,NombreSFCC = pdpBinProd.Desc_Marca
		,Bit_Cred = 0 -- Para las tarjetas Omonel PVS pide que todas sean Cred = 0 
	FROM
		#formas
	JOIN
		pdpBin
	ON
			pdpBin.Id_Cve_Bin = LEFT(#formas.Referencia,6)
	JOIN
		pdpBinProd
	ON
			pdpBinProd.Id_Cve_Bin = pdpBin.Id_Cve_Bin
		AND	pdpBinProd.Desc_Marca = 'OMONEL'
--
-- Forma de Pago DE y ED
--
DECLARE		 @OriginalOrderNo				VARCHAR(20)
			,@c_redeemedCash				MONEY
			,@c_redeemedElectronicMoney		MONEY

SELECT		@OriginalOrderNo = LTRIM(RTRIM(RIGHT('00000000' + CONVERT(VARCHAR,@OrderPOS),8)))

SELECT		 @c_redeemedCash				= c_redeemedCash
			,@c_redeemedElectronicMoney		= c_redeemedElectronicMoney
	FROM	dbo.getRedeemeInfo( @OriginalOrderNo )

SELECT		 @c_redeemedCash				= ISNULL( @c_redeemedCash , 0 )
			,@c_redeemedElectronicMoney		= ISNULL( @c_redeemedElectronicMoney , 0 )

UPDATE		#Formas
	SET		Valor		= Valor - @c_redeemedElectronicMoney - @c_redeemedCash

-- 20250307
-- Se retira esta regla que dice que si la orden se pagó con una sola forma de pago y es Omonel, se dejan fuera los artículos de garantía
-- ya que estos no forman parte de la tabla OrderFacts_POSProducts
--
--if (select count(*) from OrderFacts_POSMethodPayment a WHERE a.OrderNo = @Id_Num_Orden) = 1
--	and (select top (1) a.PaymentMethodDesc from OrderFacts_POSMethodPayment a WHERE a.OrderNo = @Id_Num_Orden) = 'OMONEL'
--	BEGIN
--		UPDATE	#Formas
--			SET Valor = 
--			(SELECT sum(a.PosQuantity * a.PosPriceOfferSale) FROM OrderFacts_POSProducts a WHERE a.OrderNo = @Id_Num_Orden)
--	END

IF ( @c_redeemedElectronicMoney > 0 OR @c_redeemedCash > 0 )
  BEGIN
	DECLARE @CustomerPersonalId	VARCHAR(40)
	SELECT @CustomerPersonalId = CustomerPersonalId FROM   dbo.OrderFacts_POSClient WHERE OrderNo = @Id_Num_Orden
  END

IF ( @c_redeemedElectronicMoney > 0 )
	INSERT INTO		#formas
		SELECT	 @Id_Num_Orden
				,12	
				,'Dinero Electronico'
				,'Dinero Electronico'
				,@c_redeemedElectronicMoney
				,@CustomerPersonalId
				,0

IF ( @c_redeemedCash > 0 )
	INSERT INTO		#formas
	  SELECT
				 @Id_Num_Orden
				,12
				,'Efectivo Disponible'
				,'Efectivo Disponible'
				,@c_redeemedCash	
				,@CustomerPersonalId
				,0

	-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO	
	update #formas
		set Referencia=''
		where NombreSFCC in ('Efectivo Disponible','Dinero Electronico')

	-- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO	 
	set @primero = 0;
	select top (1) @primero = a.consecutivo from #formas a order by a.consecutivo;

-- HU.49521 - PAGO DE SERVICIOS
-- 20241212 se elimina esta lógica para pago de servicios y se deja la Forma de Pago Original
--if @ueType = 'PSER'
--	update #formas
--		set NombrePOS='PSER', NombreSFCC='PSER';

-- TASK.50214.DE y EF colocar la referencia.
declare @ref varchar(40)
 select 
	@ref = PC.CustomerPersonalId
 FROM  
  dbo.OrderFacts_POSClient (NOLOCK) AS PC  
 WHERE PC.OrderNo = @Id_Num_Orden  
 set @ref = ISNULL(@ref, '');

--
-- Salida
--

SELECT
		 CASE F.NombrePOS 
				WHEN 'Efectivo' THEN 0
				WHEN 'Efectivo/Vales' THEN 0
				WHEN 'Vales/Efectivo' THEN 0
				WHEN 'Tarjeta de crédito contra entrega' THEN 0
				WHEN 'TD VS Entrega' THEN 0
				WHEN 'Vales' THEN 0
				WHEN 'Vales de despensa' THEN 0
				WHEN 'Tarjeta de crédito' THEN 13 -- Entran tarjetas de Crédito y Débito
				WHEN 'PayPal' THEN 54
				WHEN 'PIS' THEN 42
				WHEN 'OMONEL' THEN 12
				WHEN 'Dinero Electronico' THEN 12
				WHEN 'Efectivo Disponible' THEN 12
				WHEN 'DEUNA - OXXOPAY' THEN 85
				WHEN 'DEUNA - kueski' THEN 86
				WHEN 'PSER' THEN 00
		 END AS Id_Num_FP
		-- 2023-11-16 - se resta el monto de marketec
		--,F.Valor   AS Imp_Pago
		-- change.45622.Venta de Garantías. Ajuste a información que OMS envía a PVS.
		,CASE 
			WHEN F.consecutivo = @primero THEN CAST((F.Valor - @imp_mkt) AS DECIMAL(18,2))
			ELSE CAST(F.Valor AS DECIMAL(18,2))
		 END AS Imp_Pago
		-- 2023-11-16 - se agrega el monto de marketec
		,CASE WHEN F.Referencia = '0'
			  THEN ''  
			  WHEN F.NombrePOS IN ('Dinero Electronico', 'Efectivo Disponible')
			  THEN @ref
			  ELSE F.Referencia 
		 END AS Referencia 
		 -- HU.50214 - MOSTRAR LAS DIFERENTES FORMAS DE PAGO	
		,CASE  
			WHEN F.Bit_Cred = 1 THEN '02' -- Tarjeta de Credito
			WHEN F.Bit_Cred = 0 THEN --'01' -- Tarjeta de Debito
					 CASE F.NombrePOS
							WHEN 'OMONEL' THEN '01'
							WHEN 'Dinero Electronico' THEN '02'
							WHEN 'Efectivo Disponible' THEN '03'
							ELSE '01' -- Tarjeta de Debito
					 END
			WHEN F.Bit_Cred IS NULL THEN
					 CASE F.NombrePOS
							WHEN 'Dinero Electronico' THEN '02'
							WHEN 'Efectivo Disponible' THEN '03'
					 END
		 END
		 AS Cve_Adq
	FROM	#formas F  
	WHERE	F.Valor >	 0  
	ORDER BY F.FormaPago  
  
-- *********************************************************************  
-- ***** SELECT #6 DE CUPON MARKETEC - 2023-03-23
-- *********************************************************************  
	
	SELECT C.CouponCode
	FROM OrderFacts_Coupon C
	WHERE C.OrderNo=@Id_Num_Orden
	AND LEN(RTRIM(LTRIM(C.CouponCode))) > 0
	ORDER BY C.CouponCode;
-- *********************************************************************  
-- *********************************************************************  


-- *********************************************************************  
-- ***** SELECT #7 DE tiempo aire - 2023-06-08
-- *********************************************************************  
	declare @ordernumber int = (select top (1) a.OrderNoPOS from OrderFacts_UE_Transition a where a.OrderNo=@Id_Num_Orden);
	
	SELECT 
		@Id_Num_Orden           [orderNo],
		C.Producto              [productId],
		c.telefonia             [telefonia],
		C.telefono              [telefono],
		C.providerTransactionId [providerTransactionId],
		dbo.ReplaceSpacesWithPipe(C.Receipt)				[Receipt]
		--C.Receipt				[Receipt]
	FROM OrderFacts_VentaTiempoAire C
	WHERE C.orderNumber=@ordernumber
	ORDER BY C.Producto;
-- *********************************************************************  
-- *********************************************************************  


-- *********************************************************************  
-- ***** SELECT #8 DE CUPONES MARKETEC - HU_21469_CONTABILIZACION - 2023-08-18
-- *********************************************************************  

--	if @ueType in ('CEDIS','DSV','DST')
	BEGIN

		select b.Orden, 
		b.FolioProm, 
		b.Cve_Prefijo_Marketec, 
		SUM(b.ImporteProm) [ImporteProm],
		b.CodBarraArtConPromocion
		
		from (
			select 
					a.orderNo                            [Orden],
					convert(nvarchar(20), a.CouponCode)  [FolioProm],
					'08'                                 [Cve_Prefijo_Marketec],
					ABS(a.ValueByOrder)                  [ImporteProm],
					a.barcodeSOT                         [CodBarraArtConPromocion]
			from @tblMK a
			where a.nivel = 1
			and a.ValueByOrder>0
		
			UNION ALL

			select 
					a.orderNo                            [Orden],
					convert(nvarchar(20), a.CouponCode)  [FolioProm],
					'08'                                 [Cve_Prefijo_Marketec],
					ABS(a.ValueByItem)                   [ImporteProm],
					a.barcodeSOT                         [CodBarraArtConPromocion]
			from @tblMK a
			where a.nivel = 2
			and a.ValueByItem>0
		) b
		group by b.Orden, b.FolioProm, b.Cve_Prefijo_Marketec, b.CodBarraArtConPromocion
		Order by b.Orden, b.CodBarraArtConPromocion

	END
-- *********************************************************************  
-- *********************************************************************  


-- *********************************************************************  
-- ***** SELECT #9 venta de garantias - 2024-04-29
-- *********************************************************************  
	select c.Num_CodBarra_Art,
	       c.Num_CodBarra_Garantia,
		   c.Importe_Garantia
	from @tblwarranties c
	order by c.Num_CodBarra_Garantia;
-- *********************************************************************  
-- *********************************************************************  


-- *********************************************************************  
-- ***** Recordset #10:  HU.47943.farmacia especializada
-- *********************************************************************  
	select 
		a.Barcode Num_CodBarra,
		a.Receta RetieneReceta,
		a.Antibiotico BitAntibiotico,
		a.Psicotropico BitPsicotropico,
		isnull(a.CveCedula,' ') Id_Cve_Cedula,
		a.Cedula,
		a.Nombre,
		a.Direccion,
		a.FecReceta Fec_Movto
	from OrderFacts_UE_ItemsToSupply a
	join OrderFacts_POSProducts b 
		on b.OrderNo = a.OrderNo 
		and b.PosBarcode = a.Barcode
	where a.OrderNo = @Id_Num_Orden
	and (not a.Cedula is null and not a.Nombre is null and (a.Receta = 1 OR a.Antibiotico = 1 OR a.Psicotropico = 1));

-- *********************************************************************  
-- ***** Recordset #11: HU.49521 - PAGO DE SERVICIOS - ServicioExterno
-- *********************************************************************  
	SELECT 
		SUBSTRING(a.ClaveCuenta, 1, 100) ClaveCuenta,
		SUBSTRING(a.Descripcion, 1, 30) Descripcion ,
		a.NumeroCajaGral,
		a.Importe,
		a.ImporteCmsn,
		SUBSTRING(a.DescripcionImpCmsn, 1, 50) DescripcionImpCmsn,
		SUBSTRING(b.CodigoRespuesta, 1, 50) CodigoRespuesta,
		SUBSTRING(b.CodigoAutorizacion, 1, 12) CodigoAutorizacion,
		b.MensajeAutorizacion
	FROM OrderFacts_ServiciosItems a
	join OrderFacts_ServiciosExterno b 
		on b.OrderNo = a.OrderNo
	where a.OrderNo = @Id_Num_Orden;

-- *********************************************************************  
-- ***** Recordset #12: HU.49521 - PAGO DE SERVICIOS - ServExtMsjeProv
-- *********************************************************************  
	SELECT 
		a.NumeroRenglon,
		a.NumeroTiporenglon,
		a.Mensaje
	FROM OrderFacts_ServiciosMsjeProv a
	where a.OrderNo = @Id_Num_Orden;

-- *********************************************************************  
-- ***** Recordset #13: HU.49521 - PAGO DE SERVICIOS - ServExtMsjeTicket
-- *********************************************************************  
	SELECT 
		a.NumeroSeccion,
		a.NumeroRenglon,
		a.Mensaje,
		a.Alineacion,
		a.EsBold,
		a.EsDobleAlto
	FROM OrderFacts_ServiciosMsjeTicket a
	where a.OrderNo = @Id_Num_Orden;

-- *********************************************************************  
-- *********************************************************************  
