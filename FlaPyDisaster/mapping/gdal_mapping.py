try:
    # conda install of gdal
    import gdal
except ImportError:
    # pip pygdal install of gdal
    from osgeo import gdal

import numpy as np
import os

def save_array_to_raster(val_array, file_uri, overwrite=False, bands=1, write_band=1):
    """
    Convert an array to a raster and save it
    :param val_array: numpy array
    :param file_uri:
    :param overwrite:
    :param bands:
    :param write_band:
    :return:
    """
    if overwrite and os.path.isfile(file_uri):
        os.remove(file_uri)

    cols = val_array.shape[1]
    rows = val_array.shape[0]

    # Create memory raster
    mem_driver = gdal.GetDriverByName('MEM')
    mem_raster = mem_driver.Create('', cols, rows, bands, gdal.GDT_Byte)

    # Write array to memory raster
    outband = mem_raster.GetRasterBand(write_band)
    outband.WriteArray(val_array)

    # write alpha as 255 if making a 4-band array
    if bands == 4:
        a_band_array = np.full((val_array.shape[0], val_array.shape[1]), 255, dtype=int)
        a_band = mem_raster.GetRasterBand(4)
        a_band.WriteArray(a_band_array)

    # make physical png raster from memory raster
    png_driver = gdal.GetDriverByName('PNG')
    out_raster = png_driver.CreateCopy(file_uri, mem_raster, 0)

    # Explicitly close rasters and bands, might not be necessary
    mem_raster.FlushCache()
    outband.FlushCache()
    out_raster.FlushCache()


def read_raster_to_array(raster_uri):
    raster = gdal.Open(raster_uri)
    band = raster.GetRasterBand(1)
    array = band.ReadAsArray()
    return array
