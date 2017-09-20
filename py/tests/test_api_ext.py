# coding: utf-8

"""
    Aeternity Epoch

    This is the [Aeternity](https://www.aeternity.com/) Epoch API.

    OpenAPI spec version: 1.0.0
    Contact: apiteam@aeternity.com
    Generated by: https://github.com/swagger-api/swagger-codegen.git
"""


from __future__ import absolute_import

import os
import sys
import unittest

import swagger_client
from swagger_client.rest import ApiException
from swagger_client.apis.external_api import ExternalApi
from swagger_client.api_client import ApiClient


class TestExternalApi(unittest.TestCase):
    EXT_API = {
        'dev1': ExternalApi(ApiClient(host='localhost:3013/v1')),
        'dev2': ExternalApi(ApiClient(host='localhost:3023/v1')),
        'dev3': ExternalApi(ApiClient(host='localhost:3033/v1')),
    }
    """ ExternalApi unit test stubs """

    def test_get_block(self):
        """
        Test case for get_block

        
        """
        pass

    def test_get_top(self):
        """
        Test case for get_top

        
        """
        api = self.EXT_API['dev1']
        ping = api.get_top()

    def test_ping(self):
        """
        Test case for ping

        
        """
        api = self.EXT_API['dev1']
        ping = api.ping()

    def test_put_block(self):
        """
        Test case for put_block

        
        """
        pass


if __name__ == '__main__':
    unittest.main()

