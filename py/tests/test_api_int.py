# coding: utf-8

from __future__ import absolute_import

import unittest

from swagger_client.api_client import ApiClient
from swagger_client.apis.internal_api import InternalApi
from swagger_client.models.spend_tx import SpendTx


class TestInternalApi(unittest.TestCase):
    INT_API = {
        'dev1': InternalApi(ApiClient(host='localhost:3113/v1')),
        'dev2': InternalApi(ApiClient(host='localhost:3123/v1')),
        'dev3': InternalApi(ApiClient(host='localhost:3133/v1')),
    }

    def test_spend_tx_creation(self):
        """
        Very dummy spend transaction creation test,
        Checking only that the API does not throw an error now.
        TODO: Rewrite to proper user story, when created transaction is propagated to peers etc..

        """
        api = self.INT_API['dev1']
        spend_tx_obj = SpendTx(
            recipient_pubkey="BAAggMEhrC3ODBqlYeQ6dk00F87AKMkV6kkyhgfJ/luOzGUC+4APxFkVgAYPai3TjSyLRObv0GeDACg1ZxwnfHY=",
            amount=0,
            fee=0)
        api.post_spend_tx(spend_tx_obj)
