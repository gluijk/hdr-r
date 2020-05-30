# Fusión HDR de imágenes con R
# www.overfitting.net
# https://www.overfitting.net/2018/07/fusion-hdr-de-imagenes-con-r.html

library(tiff)


# LEER FOTOGRAFÍAS
# Revelado lineal con DCRAW: dcraw -v -w -o 2 -4 -T *.CR2
img1=readTIFF("raw1.tiff", native=F, convert=F)
img2=readTIFF("raw2.tiff", native=F, convert=F)


# CÁLCULO EXPOSICIÓN RELATIVA
MIN=2^(-5)  # Desde -5EV...
MAX=0.95  # ...hasta 95%
indices=which(img1>=MIN & img1<=MAX & img2>=MIN & img2<=MAX)
exprel=img2[indices]/img1[indices]
f=median(exprel)  # Factor corrector de exposición

# Histograma de exposiciones relativas
hist(exprel[exprel>=10 & exprel<=22],
    main='Relative exposure histogram', xlab='Linear relative exposure',
    breaks=seq(10, 22, length.out=800))
abline(v=16, col='gray', lty='dotted')
abline(v=f, col='red')

mapacalc=img1*0
mapacalc[indices]=1  # 1=lo que ha participado en el cálculo
writeTIFF(mapacalc, "mapacalc.tif", bits.per.sample=8, compression="LZW")
solape=length(indices)/length(img1)  # % información entró en el cálculo


# FUSIÓN HDR
hdr=img1  # Partimos de la foto menos expuesta
indices=which(img2<=MAX)  # Niveles RGB a obtener de la foto más expuesta
hdr[indices]=img2[indices]/f  # Los sobreescribimos igualando exposición
writeTIFF(hdr^(1/2.2), "hdr.tif", bits.per.sample=16, compression="LZW")

mapafusion=img1*0
mapafusion[-indices]=1  # 1=lo obtenido de la foto menos expuesta
writeTIFF(mapafusion, "mapafusion.tif", bits.per.sample=8, compression="LZW")
mejora=length(indices)/length(img1)  # % información foto más expuesta
