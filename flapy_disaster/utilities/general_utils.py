from enum import Enum

# from flapy_disaster.app import flapy_app
import werkzeug as wk
import os


# class Web:
#     @staticmethod
#     def get_web_file_uri(web_file):
#         """
#         Save a file upload to the app UPLOAD_FOLDER
#         :param web_file: flask.request.files[] web_file object from request
#         :return: str local uri
#         """
#         filename = wk.secure_filename(web_file.filename)
#         web_file.save(os.path.join(flapy_app.config['UPLOAD_FOLDER'], filename))
#         return os.path.join(flapy_app.config['UPLOAD_FOLDER'], filename)


class FDEnum(Enum):
    @classmethod
    def has_value(cls, value):
        return any(value == item.value for item in cls)
