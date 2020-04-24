import logging

from abc import ABC, abstractmethod
from typing import Dict

from marshmallow import Schema, fields, INCLUDE, ValidationError
from marshmallow.schema import SchemaMeta

from flapy_disaster.utilities.general_utils import FDEnum

logger: logging.Logger = logging.getLogger(__name__)


class Configurations(FDEnum):
    aws = "aws"
    mongo = "mongo"
    app_config = "app_config"


class Configuration(ABC):
    def __init__(self, configuration_data: Dict):
        self.configuration_data = configuration_data
        self._validate(configuration_data)

    @abstractmethod
    def _validate(self, configuration_data: Dict) -> bool:
        pass

    def _run_validate(self, check_schema: SchemaMeta, configuration_data: Dict) -> bool:
        validation_errors: Dict = check_schema(unknown=INCLUDE).validate(configuration_data)

        if validation_errors != {}:
            logger.error(f"configuration load error in {self.__class__}: {validation_errors}")
            raise ValidationError(validation_errors)

        return validation_errors == {}


class AWSConfiguration(Configuration):
    def __init__(self, configuration_data: Dict):
        super().__init__(configuration_data)
        self.access_key: str = configuration_data["access_key"]
        self.secret_key: str = configuration_data["secret_key"]

    def _validate(self, configuration_data: Dict) -> bool:
        class _AWSSchema(Schema):
            access_key = fields.String(required=True)
            secret_key = fields.String(required=True)

        return self._run_validate(_AWSSchema, configuration_data)


class MongoConfiguration(Configuration):
    def __init__(self, configuration_data: Dict):
        super().__init__(configuration_data)
        self.host: str = configuration_data["host"]
        self.port: str = configuration_data["port"]

    def _validate(self, configuration_data: Dict) -> bool:
        class _MongoSchema(Schema):
            host = fields.String(required=True)
            port = fields.Integer(required=True)

        return self._run_validate(_MongoSchema, configuration_data)


class AppConfigContainer(Configuration):
    def __init__(self, configuration_data: Dict):
        super().__init__(configuration_data)

        self.title: str = configuration_data["title"]
        self.app_version: str = configuration_data["app_version"]
        self.openapi_version: str = configuration_data["openapi_version"]

    def _validate(self, configuration_data: Dict) -> bool:
        class _AppConfigSchema(Schema):
            title = fields.String(required=True)
            app_version = fields.String(required=True)
            openapi_version = fields.String(required=True)

        return self._run_validate(_AppConfigSchema, configuration_data)
