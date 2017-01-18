# http://stackoverflow.com/questions/902761/saving-a-numpy-array-as-an-image
def write_png(buf, width, height):
    """ 
    :param buf: must be bytes or a bytearray in Python3.x, a regular string in Python2.x in the format RGBARGBA...
    :param width: width of image
    :param height: height if image
    :returns: binary string to write to ifle
    :usage: data = write_png(buf, 64, 64)
            with open("my_image.png", 'wb') as fd:
            fd.write(data)
    """
    import zlib
    import struct

    # reverse the vertical line order and add null bytes at the start
    width_byte_4 = width * 4
    raw_data = b''.join(b'\x00' + buf[span:span + width_byte_4]
                        for span in range((height - 1) * width * 4, -1, - width_byte_4))

    def png_pack(png_tag, data):
        chunk_head = png_tag + data
        return (struct.pack("!I", len(data)) +
                chunk_head +
                struct.pack("!I", 0xFFFFFFFF & zlib.crc32(chunk_head)))

    return b''.join([
        b'\x89PNG\r\n\x1a\n',
        png_pack(b'IHDR', struct.pack("!2I5B", width, height, 8, 6, 0, 0, 0)),
        png_pack(b'IDAT', zlib.compress(raw_data, 9)),
        png_pack(b'IEND', b'')])
