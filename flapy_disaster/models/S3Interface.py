from io import BytesIO
from pathlib import PurePath
from typing import AnyStr, BinaryIO, TextIO, Dict

import boto3
from botocore.client import BaseClient
from botocore.response import StreamingBody

from flapy_disaster.utilities.general_utils import FDEnum


class FileEncodings(FDEnum):
    utf8 = "utf-8"
    png = "png"
    none = "none"
    auto = "auto"


class S3Interface:
    def __init__(self, access_key: str, secret_key: str):
        self.access_key: str = access_key
        self.secret_key: str = secret_key

        self.client: BaseClient = boto3.client("s3",
                                               aws_access_key_id=self.access_key,
                                               aws_secret_access_key=self.secret_key)

    def get_file(self, bucket: str, file_uri: PurePath, encoding: FileEncodings = FileEncodings.none) -> AnyStr:
        s3_file_object = self.client.get_object(Bucket=bucket, Key=file_uri.as_posix())
        s3_body_object: StreamingBody = s3_file_object["Body"]

        file_contents: AnyStr = s3_body_object.read()
        s3_body_object.close()

        if encoding is not None and encoding != FileEncodings.none:
            file_contents = file_contents.decode(encoding)

        return file_contents

    def put_file(self, bucket: str, file_uri: PurePath, file_content: [AnyStr, TextIO, BinaryIO]) -> Dict:
        response: Dict = self.client.put_object(Body=file_content, Bucket=bucket, Key=file_uri.as_posix())
        return response

    # def get_image_file(self, bucket: str, file_uri: PurePath) -> bytes:
    #     return self.get_file(bucket, file_uri, FileEncodings.png)

