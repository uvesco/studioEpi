r.in.gdal input=/home/umberto/Documenti/importanza_1_sync/unaapi/Sperimentazioni/monitoraggio_varroa/geodata/gtopo30/gtopo30_europa.tif output=gtopo30_europa
r.colors map=gtopo30_europa@dati_base color=srtm
##cambio mapset
r.proj input=gtopo30_europa location=wgs84_latlon mapset=dati_base
r.out.tiff -t input=gtopo30_europa@dati_base output=gtopo30_europa_utm.tif compression=lzw

