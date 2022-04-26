data Producto = UnProducto {
    nombre :: String,
    precio :: Int
}deriving (Show,Eq,Ord) 

bufanda :: Producto
bufanda = UnProducto "bufanda" 1500

precioTotal :: Producto -> Int -> Int -> Int -> Int
precioTotal producto cantidad descuento envio = precio (aplicarCostoDeEnvio productoConDescuentoYCantidad envio)
    where productoConDescuentoYCantidad = producto {precio = (*cantidad).precio.aplicarDescuento producto $ descuento}

productoDeElite :: Producto -> Bool
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && not (productoCorriente producto)

aplicarDescuento :: Producto -> Int -> Producto
aplicarDescuento producto descuento = producto {precio = precio producto - div (precio producto * descuento) 100} 

entregaSencilla :: String -> Bool
entregaSencilla dia = even.length $ dia

descodiciarProducto :: Producto -> Producto
descodiciarProducto producto = producto {nombre = take 10 (nombre producto)}

productoDeLujo :: Producto -> Bool
productoDeLujo producto = elem 'x' (nombre producto) || elem 'z' (nombre producto)

aplicarCostoDeEnvio :: Producto -> Int -> Producto
aplicarCostoDeEnvio producto costoEnvio = producto {precio = precio producto + costoEnvio}

productoCodiciado :: Producto -> Bool
productoCodiciado producto = (length.nombre $ producto) > 10

productoCorriente :: Producto -> Bool
productoCorriente producto = elem (head.nombre $producto) "aeiouAEIOU"

productoXL :: Producto -> Producto
productoXL producto = producto {nombre = nombre producto ++ "XL"} 

versionBarata :: Producto -> Producto
versionBarata producto = producto { nombre = reverse.nombre.descodiciarProducto $ producto}