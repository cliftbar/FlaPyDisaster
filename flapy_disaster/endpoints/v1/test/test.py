from pathlib import Path, PurePath
from typing import Dict

from automd.decorators import automd
from flask_restful import Resource

from flapy_disaster.app import config_loader_key
from flask import current_app as flapy_app

from flapy_disaster.models.S3Interface import S3Interface
from flapy_disaster.utilities.ConfigurationContainers import Configurations, AWSConfiguration
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader
from flapy_disaster.utilities.general_image import image_bytes_to_array

open_api_catalog_tag: Dict = {"name": "Test"}


class Test(Resource):
    @automd(summary="Random testing endpoint",
            tags=[open_api_catalog_tag])
    def get(self):

        test_png: bytes = Path("documentation\Hurricane\MATTHEW_2016_Sample.png").read_bytes()
        config: ConfigurationLoader = flapy_app.config[config_loader_key]
        aws_config: AWSConfiguration = AWSConfiguration(config.get_config(Configurations.aws))
        s3_interface: S3Interface = S3Interface(aws_config.access_key, aws_config.secret_key)
        # s3_interface.put_file("fd-events", PurePath("test/test.png"), test_png)
        fi = s3_interface.get_file("fd-events", PurePath("test/test.png"))

        raster_array = image_bytes_to_array(fi)

        return "OK"
