from matplotlib import colors
import globes as gb
import ast


class ColorPalettes:
    simple_escalating_5 = [colors.cnames['blue'], colors.cnames['green'], colors.cnames['yellow'], colors.cnames['orange'], colors.cnames['red']]

    @staticmethod
    def hex_to_rgb(palette, scalar=1, as_int=True):
        """
        convert a list of hex strings to rgb values scaled to the input scalar (default is 1)
        :param palette: List of color hex strings.  Expects one of the class Palettes, but can be generated through other means
        :param scalar: Number to scale the rgb values.  255 gives the channel values scaled from 0-255
        :param as_int: Boolean whether to truncate numbers to an int or not.
        """
        return list(map(lambda x: list(map(lambda y: int(y * scalar) if as_int else (y * scalar), colors.hex2color(x))), palette))

    @staticmethod
    def even_value_breaks(values, num_breaks):
        """
        Returns a list of values that has the breaks for a (relatively) even split into bins
        :param values: A sorted list of values
        :param num_breaks: The number of desired bins
        :returns: List of values, each value is the top of its bin range
        :example: usage of breaks (break_vals) to find bin number of a value
            for pos in range(break_vals):
                if value <= break_vals[pos]:
                    return pos
        """
        break_num = int(len(values) / num_breaks)
        ret_breaks = []
        pos = break_num
        i = 0
        for pos in range(len(values)):
            if i + 1 == break_num:
                ret_breaks.append(values[pos])
                i = 0
            else:
                i += 1

        return ret_breaks


def get_named_color_schemes_from_config():
    """
    Read the Global config object for color scheme names
    Eventually, make this any config file
    :return: list of str color scheme names
    """
    ret = list(dict(gb.GlobalConfig.items('DefaultStyles')).keys())

    if 'UserStyles' in gb.GlobalConfig.sections():
        user_list = list(dict(gb.GlobalConfig.items('UserStyles')).keys())
        ret += user_list
    print(ret)
    ret.sort(key=natural_key)
    return ret


def natural_key(string_):
    import re
    """See http://www.codinghorror.com/blog/archives/001018.html"""
    return [int(s) if s.isdigit() else s for s in re.split(r'(\d+)', string_)]

def get_named_color_scheme_colors_from_config(name):
    """
    Read the Global config file and get the color scheme associated with the name.  System default styles are prioritized
    Eventually, make this any config file
    :param name: str
    :return: list str list of hex codes for color scheme
    """
    color_scheme = None
    # split_name = name.split('!', maxsplit=1)[1]
    if name in dict(gb.GlobalConfig.items('DefaultStyles')).keys():
        color_scheme = ast.literal_eval(gb.GlobalConfig.get('DefaultStyles', name))
    elif name in dict(gb.GlobalConfig.items('UserStyles')).keys():
        color_scheme = ast.literal_eval(gb.GlobalConfig.get('UserStyles', name))

    print(color_scheme)
    return color_scheme

def save_named_color_scheme_to_config(name, hex_color_scheme):
    print('Saving Color Scheme')
    if 'UserStyles' not in gb.GlobalConfig.sections():
        gb.GlobalConfig.add_section('UserStyles')
    styles_dict = dict(map(lambda x: (x.split('!', maxsplit=1)[1], x.split('!', maxsplit=1)[0]), list(dict(gb.GlobalConfig.items('UserStyles')).keys())))
    cnt = len(styles_dict)

    # Update if style name already exists
    if name in styles_dict.keys():
        cnt = styles_dict[name]
    gb.GlobalConfig.set('UserStyles', str(cnt) + "!" + name,  str(hex_color_scheme))
    gb.save_config()