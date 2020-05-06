import time

from flapy_disaster.services.hurricane.HurricaneCatalog import HurricaneEvent
from flapy_disaster.services.hurricane.HurricaneService import HurricaneService
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader


def main():
    start = time.time()
    event_id: str = "2018_14_MICHAEL_AL"
    config: ConfigurationLoader = ConfigurationLoader()
    hurricane_service: HurricaneService = HurricaneService(config)
    event: HurricaneEvent = hurricane_service.get_event(event_id)
    event_result = hurricane_service.calculate_event(event)
    end = time.time()
    print(f"{len(event_result)}, {int(end - start)}s")


if __name__ == "__main__":
    main()
