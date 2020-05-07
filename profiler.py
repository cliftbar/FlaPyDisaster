import time
from io import BytesIO
from pathlib import PurePath
from typing import List, Tuple

from PIL import Image as img
from PIL.Image import Image
from numpy import ndarray

from flapy_disaster.models.S3Interface import S3Interface
from flapy_disaster.services.hurricane.HurricaneCatalog import HurricaneEvent
from flapy_disaster.services.hurricane.HurricaneService import HurricaneService
from flapy_disaster.utilities.ConfigurationContainers import AWSConfiguration, Configurations
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader
from flapy_disaster.utilities.general_objects import LatLonGrid


def main():
    start = time.time()
    event_id: str = "2018_14_MICHAEL_AL"
    config: ConfigurationLoader = ConfigurationLoader()
    hurricane_service: HurricaneService = HurricaneService(config)
    aws_config: AWSConfiguration = AWSConfiguration(config.get_config(Configurations.aws))
    s3_interface: S3Interface = S3Interface(aws_config.access_key, aws_config.secret_key)

    event: HurricaneEvent = hurricane_service.get_event(event_id)
    lat_lon_grid: LatLonGrid = LatLonGrid.from_bounding_box(hurricane_service.create_bounds_from_track(event),
                                                            100,
                                                            100)
    event_results: List[Tuple[float, float, int]] = hurricane_service.calculate_event(event, lat_lon_grid)

    shaped_nd: ndarray = hurricane_service.event_results_to_numpy(event_results, lat_lon_grid)

    event_raster: Image = img.fromarray(shaped_nd).convert(mode="L")
    # mem_file
    event_raster.save("test.png")

    mem_file: BytesIO = BytesIO()
    event_raster.save(mem_file, format="PNG")
    mem_file.seek(0)

    s3_interface.put_file("fd-events", PurePath("test/test.png"), mem_file)

    end = time.time()
    print(f"{len(event_results)}, {int(end - start)}s")


if __name__ == "__main__":
    main()
