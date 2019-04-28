# Fusi�n HDR de im�genes con R
# www.datosimagensonido.com

library(tiff)


# LEER FOTOGRAF�AS
# Revelado lineal con DCRAW: dcraw -v -w -o 2 -4 -T *.CR2
img1=readTIFF("raw1.tiff", native=F, convert=F)
img2=readTIFF("raw2.tiff", native=F, convert=F)


# C�LCULO EXPOSICI�N RELATIVA
MIN=2^(-5)  # Desde -5EV...
MAX=0.95  # ...hasta 95%
indices=which(img1>=MIN & img1<=MAX & img2>=MIN & img2<=MAX)
exprel=img2[indices]/img1[indices]
f=median(exprel)  # Factor corrector de exposici�n

# Histograma de exposiciones relativas
hist(exprel[exprel>=10 & exprel<=22],
    main='Relative exposure histogram', xlab='Linear relative exposure',
    breaks=seq(10, 22, length.out=800))
abline(v=16, col='gray', lty='dotted')
abline(v=f, col='red')

mapacalc=img1*0
mapacalc[indices]=1  # 1=lo que ha participado en el c�lculo
writeTIFF(mapacalc, "mapacalc.tif", bits.per.sample=8, compression="LZW")
solape=length(indices)/length(img1)  # % informaci�n entr� en el c�lculo


# FUSI�N HDR
hdr=img1  # Partimos de la foto menos expuesta
indices=which(img2<=MAX)  # Niveles RGB a obtener de la foto m�s expuesta
hdr[indices]=img2[indices]/f  # Los sobreescribimos igualando exposici�n
writeTIFF(hdr^(1/2.2), "hdr.tif", bits.per.sample=16, compression="LZW")

mapafusion=img1*0
mapafusion[-indices]=1  # 1=lo obtenido de la foto menos expuesta
writeTIFF(mapafusion, "mapafusion.tif", bits.per.sample=8, compression="LZW")
mejora=length(indices)/length(img1)  # % informaci�n foto m�s expuesta