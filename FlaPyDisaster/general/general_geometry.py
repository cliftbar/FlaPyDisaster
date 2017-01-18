import math


def hello():
    """
    Hello message to get some info on the package.
    """
    ret_string = "This is the general geometry package! This will contain some help text."
    print(ret_string)


def find_hypotenuse_right_triangle(side1, side2):
    """
    Find the hypotenuse of a triangle.
    :returns: Hypotenuse or -1 if error
    """
    if side1 != 0 and side2 != 0:
        return math.sqrt((side1 * side1) + (side2 * side2))
    else:
        return -1


def find_bottom_right_triangle(height, hypotenuse):
    """
    Find third non-hypotenuse side of a triangle.
    :returns: Side length or -1 if error
    """
    if height != 0 and hypotenuse != 0 and hypotenuse > height:
        return math.sqrt((hypotenuse * hypotenuse) - (height * height))
    else:
        return -1
