class BoundingBox:
    """
    Class to represent a bounding box.  Contians a Top and Bottom Latitude, and a Left and Right Longitude.
    Also contains some methods for describing the Bounding Box, such as a get width and height function.
    Assumes lat and long are -180 to 180
    :param top_lat_y: Top/maximum latitude, Y axis
    :param bot_lat_y: Bottom/minimum latitude, Y axis
    :param right_lon_x: Right/maximim longitude, X axis
    :param left_lon_x: Left/minimum logitude, X axis
    :method get_width: Returns the width of the bounding box in degrees (right minus left)
    :method get_height: Returns the height of the bounding box in degrees (top minus bottom)
    :method translate: STUB Returns a copy of the current bounding box, translated by x, y degrees 
    """

    def __init__(self, top_lat_y, bot_lat_y, right_lon_x, left_lon_x):
        """
        Initializer for BoundingBox class
        :param top_lat_y: float deg
        :param bot_lat_y: float deg
        :param right_lon_x: float deg
        :param left_lon_x: float deg
        """
        self.top_lat_y = top_lat_y
        self.bot_lat_y = bot_lat_y
        self.right_lon_x = right_lon_x
        self.left_lon_x = left_lon_x

    def get_width(self):
        """
        Returns the width in deg of the bounding box
        :return: float deg
        """
        return self.right_lon_x - self.left_lon_x

    def get_height(self):
        """
        Returns the height in deg of the bounding box
        :return: float deg
        """
        return self.top_lat_y - self.bot_lat_y


class LatLonGrid(BoundingBox):
    """
    Class to decribe a grid in lat/long space, and methods to operate on that space.  Extends bounding box.
    Blocks describe a grid space
    :param block_per_degree_y: Number of grid spaces per degree in the Y/Latitude direction
    :param block_per_degree_x: Number of grid spaces per degree in the X/Longitude direction
    :method get_block_index: Get the index (X#, Y#) of a block given a lat/long coordinate
    :method get_lat_lon: Get the lat/long (top left of box) given a blocks index
    :method get_block_width: Get the block width in degrees (all blocks are the same width)
    :method get_block_height: Get the block height in degrees (all blocks are the same height)
    """

    def __init__(self, top_lat_y, bot_lat_y, left_lon_x, right_lon_x, block_per_degree_x, block_per_degree_y):
        """
        Initializer for a LatLon grid class
        :param top_lat_y: float deg
        :param bot_lat_y: float deg
        :param left_lon_x: float deg
        :param right_lon_x: float deg
        :param block_per_degree_x: int resolution of grid
        :param block_per_degree_y: int resolution of grid
        """
        self.block_per_degree_y = block_per_degree_y
        self.block_per_degree_x = block_per_degree_x

        super().__init__(top_lat_y, bot_lat_y, right_lon_x, left_lon_x)

    def get_block_index(self, lat_y, lon_x):
        """
        Get the block index of a point
        :param lat_y: float deg
        :param lon_x: float deg
        :return: list [x, y]
        """
        block_x = (lon_x - self.left_lon_x) * self.block_per_degree_x
        block_y = (lat_y - self.bot_lat_y) * self.block_per_degree_y

        return [block_x, block_y]

    def get_lat_lon(self, block_x, block_y):
        """
        Get the lat/long from a block index
        :param block_x: int
        :param block_y: int
        :return: list [lat, lon]
        """
        lat_y = self.get_lat(block_y)
        lon_x = self.get_lon(block_x)

        return [lat_y, lon_x]

    def get_lat(self, block_y):
        """
        Get the lat of a y index
        :param block_y: int
        :return: float def
        """
        return self.bot_lat_y + (block_y / self.block_per_degree_y)

    def get_lon(self, block_x):
        """
        Get the lon of an x index
        :param block_x: int
        :return: float deg
        """
        return self.left_lon_x + (block_x / self.block_per_degree_x)

    def get_block_width_x(self):
        """
        Get the width in blocks of the grid
        :return: int
        """
        return int(round(self.get_width() * self.block_per_degree_x))

    def get_block_height_y(self):
        """
        Get the height in blocks of the grid
        :return: int
        """
        return int(round(self.get_height() * self.block_per_degree_y))

    def get_lat_lon_list(self):
        """
        Get a list of every lat/lon point in the grid
        :return: list of list [[lat, lon] ... ]
        """
        ret_list = []
        for y in range(self.get_block_height_y()):
            for x in range(self.get_block_width_x()):
                ret_list.append([self.get_lat(y), self.get_lon(x)])

        return ret_list
