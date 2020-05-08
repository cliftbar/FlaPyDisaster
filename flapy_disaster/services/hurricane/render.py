from abc import abstractmethod
from datetime import datetime
from pathlib import PurePath
from typing import Dict

from flapy_disaster.utilities.general_objects import BoundingBox


class RenderParameters:
    def __init__(self, event_id: str, render_id: str, create_date: datetime, bounding_box: BoundingBox):
        self.event_id: str = event_id
        self.render_id: str = render_id
        self.create_date: datetime = create_date
        self.bounding_box: BoundingBox = bounding_box

    def to_dict(self) -> Dict:
        return {
            "event_id": self.event_id,
            "render_id": self.render_id,
            "create_date": self.create_date.isoformat(),
            "bounding_box": self.bounding_box.to_dict()
        }

    @staticmethod
    def from_dict(input_dict) -> "RenderParameters":
        return RenderParameters(
            input_dict["event_id"],
            input_dict["render_id"],
            input_dict["create_date"],
            BoundingBox.from_dict(input_dict["bounding_box"])
        )


class HurricaneRenderParameters:
    def __init__(self,
                 event_id: str,
                 render_id: str,
                 create_date: datetime,
                 pixels_per_degrees_x: int,
                 pixels_per_degrees_y: int,
                 bounding_box: BoundingBox,
                 render_file_path: PurePath = None):
        self.event_id: str = event_id
        self.render_id: str = render_id
        self.create_date: datetime = create_date
        self.pixels_per_degrees_x: int = pixels_per_degrees_x
        self.pixels_per_degrees_y: int = pixels_per_degrees_y
        self.bounding_box: BoundingBox = bounding_box
        self.render_file_path: PurePath = render_file_path

    def to_dict(self) -> Dict:
        return {
            "event_id": self.event_id,
            "render_id": self.render_id,
            "create_date": self.create_date.isoformat(),
            "pixels_per_degrees_x": self.pixels_per_degrees_x,
            "pixels_per_degrees_y": self.pixels_per_degrees_y,
            "bounding_box": self.bounding_box.to_dict(),
            "render_file_path": str(self.render_file_path)
        }

    @staticmethod
    def from_dict(input_dict) -> "HurricaneRenderParameters":
        return HurricaneRenderParameters(
            input_dict["event_id"],
            input_dict["render_id"],
            datetime.fromisoformat(input_dict["create_date"]),
            input_dict["pixels_per_degrees_x"],
            input_dict["pixels_per_degrees_y"],
            BoundingBox.from_dict(input_dict["bounding_box"]),
            PurePath(input_dict["render_file_path"])
        )