from general import flapy_app as fla
import configparser as cpar
import os
import shutil

# Global Variables
flapy_app = None
GlobalConfig = None
GlobalConfigFileUri = ""


def global_init():
    """
    Global intializer method, for stuff that doesn't fit well in a class
    :return: None
    """
    os.chdir(os.path.dirname(__file__))
    global flapy_app
    flapy_app = fla.FlaPyApp()

    global GlobalConfigFileUri
    GlobalConfigFileUri = r'users/user_config/user_config.ini'
    sample_config_file_uri = r'users/user_config/user_config.ini.sample'

    if not os.path.exists(GlobalConfigFileUri):
        shutil.copy2(sample_config_file_uri, GlobalConfigFileUri)

    global GlobalConfig
    GlobalConfig = cpar.ConfigParser()
    GlobalConfig.read(GlobalConfigFileUri)

    global UPLOAD_FOLDER
    UPLOAD_FOLDER = r'tmp/'

    global STATIC_FOLDER
    STATIC_FOLDER = r'static/'

    global USER_FOLDER
    USER_FOLDER = r'users/'

def save_config(config=None, file_uri=None, overwrite=False):
    """
    Save the config file
    Sample set: GlobalConfig.set('UserStyles', 'Test', str(style))
    :return: None
    """
    if config is None:
        config = GlobalConfig

    if file_uri is None:
        file_uri = GlobalConfigFileUri

    with open(file_uri, 'w') as configfile:
        config.write(configfile)

def number_config_options(section):
    """
    Get the number of config options in a section
    :param section: str
    :return: None
    """
    if section in GlobalConfig.sections():
        return len(GlobalConfig.options(section))
    else:
        return 0

def reset_application(reset_type):
    """
    Reset the application by reinitializing the FlaPyApp object
    :param reset_type: str only option now is 'hard'
    :return: None
    """
    if reset_type != 'hard':
        save_config()

    global_init()
