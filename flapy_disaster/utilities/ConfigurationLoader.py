import yaml

from pathlib import Path
from typing import Dict, Any, List, Union, Hashable

from flapy_disaster.utilities.ConfigurationContainers import Configurations


class ConfigurationLoader:
    def __init__(self, config_directory: str = "config", secrets_directory: str = "secrets", init: bool = True):
        self.config_directory: Path = Path(Path.cwd(), config_directory)
        self.secrets_file: Path = Path(Path.cwd(), secrets_directory, "secrets.yml")
        self._config_source: Dict = {}
        self._secrets_source: Dict = {}

        if init:
            self.init_config()

    def init_config(self) -> bool:
        if not self.config_directory.exists():
            raise FileNotFoundError(f"config directory {self.config_directory} does not exist")

        if not self.secrets_file.exists():
            raise FileNotFoundError(f"secret file {self.secrets_file} does not exist")

        self._secrets_source: Dict = yaml.load(self.secrets_file.read_text())

        configuration_dict: Dict[str, Any] = {}
        for configuration in self.config_directory.rglob('*.yml'):
            config_text: str = configuration.read_text()
            for key, value in self._secrets_source.items():
                config_text = config_text.replace(f"${{{key}}}", str(value))
            config: Union[Union[Dict[Hashable, Any], List[Any]], Any] = yaml.load(config_text)
            configuration_dict[configuration.stem] = config

        self._config_source = configuration_dict

        return True

    def get_config(self, config: Configurations) -> Dict:
        return self._config_source[config.value]
