# coding: utf-8

"""
    Aeternity Epoch

    This is the [Aeternity](https://www.aeternity.com/) Epoch API.  # noqa: E501

    OpenAPI spec version: 1.0.0
    Contact: apiteam@aeternity.com
    Generated by: https://github.com/swagger-api/swagger-codegen.git
"""


import pprint
import re  # noqa: F401

import six


class CreateContractData(object):
    """NOTE: This class is auto generated by the swagger code generator program.

    Do not edit the class manually.
    """

    """
    Attributes:
      swagger_types (dict): The key is attribute name
                            and the value is attribute type.
      attribute_map (dict): The key is attribute name
                            and the value is json key in definition.
    """
    swagger_types = {
        'owner': 'str',
        'nonce': 'int',
        'code': 'str',
        'vm_version': 'int',
        'deposit': 'int',
        'amount': 'int',
        'gas': 'int',
        'gas_price': 'int',
        'fee': 'int',
        'call_data': 'str'
    }

    attribute_map = {
        'owner': 'owner',
        'nonce': 'nonce',
        'code': 'code',
        'vm_version': 'vm_version',
        'deposit': 'deposit',
        'amount': 'amount',
        'gas': 'gas',
        'gas_price': 'gas_price',
        'fee': 'fee',
        'call_data': 'call_data'
    }

    def __init__(self, owner=None, nonce=None, code=None, vm_version=None, deposit=None, amount=None, gas=None, gas_price=None, fee=None, call_data=None):  # noqa: E501
        """CreateContractData - a model defined in Swagger"""  # noqa: E501

        self._owner = None
        self._nonce = None
        self._code = None
        self._vm_version = None
        self._deposit = None
        self._amount = None
        self._gas = None
        self._gas_price = None
        self._fee = None
        self._call_data = None
        self.discriminator = None

        self.owner = owner
        if nonce is not None:
            self.nonce = nonce
        self.code = code
        self.vm_version = vm_version
        self.deposit = deposit
        self.amount = amount
        self.gas = gas
        self.gas_price = gas_price
        self.fee = fee
        self.call_data = call_data

    @property
    def owner(self):
        """Gets the owner of this CreateContractData.  # noqa: E501

        Contract owner pub_key  # noqa: E501

        :return: The owner of this CreateContractData.  # noqa: E501
        :rtype: str
        """
        return self._owner

    @owner.setter
    def owner(self, owner):
        """Sets the owner of this CreateContractData.

        Contract owner pub_key  # noqa: E501

        :param owner: The owner of this CreateContractData.  # noqa: E501
        :type: str
        """
        if owner is None:
            raise ValueError("Invalid value for `owner`, must not be `None`")  # noqa: E501

        self._owner = owner

    @property
    def nonce(self):
        """Gets the nonce of this CreateContractData.  # noqa: E501

        Owner's nonce  # noqa: E501

        :return: The nonce of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._nonce

    @nonce.setter
    def nonce(self, nonce):
        """Sets the nonce of this CreateContractData.

        Owner's nonce  # noqa: E501

        :param nonce: The nonce of this CreateContractData.  # noqa: E501
        :type: int
        """

        self._nonce = nonce

    @property
    def code(self):
        """Gets the code of this CreateContractData.  # noqa: E501

        Contract's code  # noqa: E501

        :return: The code of this CreateContractData.  # noqa: E501
        :rtype: str
        """
        return self._code

    @code.setter
    def code(self, code):
        """Sets the code of this CreateContractData.

        Contract's code  # noqa: E501

        :param code: The code of this CreateContractData.  # noqa: E501
        :type: str
        """
        if code is None:
            raise ValueError("Invalid value for `code`, must not be `None`")  # noqa: E501

        self._code = code

    @property
    def vm_version(self):
        """Gets the vm_version of this CreateContractData.  # noqa: E501

        Virtual machine's version  # noqa: E501

        :return: The vm_version of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._vm_version

    @vm_version.setter
    def vm_version(self, vm_version):
        """Sets the vm_version of this CreateContractData.

        Virtual machine's version  # noqa: E501

        :param vm_version: The vm_version of this CreateContractData.  # noqa: E501
        :type: int
        """
        if vm_version is None:
            raise ValueError("Invalid value for `vm_version`, must not be `None`")  # noqa: E501
        if vm_version is not None and vm_version > 255:  # noqa: E501
            raise ValueError("Invalid value for `vm_version`, must be a value less than or equal to `255`")  # noqa: E501
        if vm_version is not None and vm_version < 0:  # noqa: E501
            raise ValueError("Invalid value for `vm_version`, must be a value greater than or equal to `0`")  # noqa: E501

        self._vm_version = vm_version

    @property
    def deposit(self):
        """Gets the deposit of this CreateContractData.  # noqa: E501

        Deposit  # noqa: E501

        :return: The deposit of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._deposit

    @deposit.setter
    def deposit(self, deposit):
        """Sets the deposit of this CreateContractData.

        Deposit  # noqa: E501

        :param deposit: The deposit of this CreateContractData.  # noqa: E501
        :type: int
        """
        if deposit is None:
            raise ValueError("Invalid value for `deposit`, must not be `None`")  # noqa: E501
        if deposit is not None and deposit < 0:  # noqa: E501
            raise ValueError("Invalid value for `deposit`, must be a value greater than or equal to `0`")  # noqa: E501

        self._deposit = deposit

    @property
    def amount(self):
        """Gets the amount of this CreateContractData.  # noqa: E501

        Amount  # noqa: E501

        :return: The amount of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._amount

    @amount.setter
    def amount(self, amount):
        """Sets the amount of this CreateContractData.

        Amount  # noqa: E501

        :param amount: The amount of this CreateContractData.  # noqa: E501
        :type: int
        """
        if amount is None:
            raise ValueError("Invalid value for `amount`, must not be `None`")  # noqa: E501
        if amount is not None and amount < 0:  # noqa: E501
            raise ValueError("Invalid value for `amount`, must be a value greater than or equal to `0`")  # noqa: E501

        self._amount = amount

    @property
    def gas(self):
        """Gets the gas of this CreateContractData.  # noqa: E501

        Contract gas  # noqa: E501

        :return: The gas of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._gas

    @gas.setter
    def gas(self, gas):
        """Sets the gas of this CreateContractData.

        Contract gas  # noqa: E501

        :param gas: The gas of this CreateContractData.  # noqa: E501
        :type: int
        """
        if gas is None:
            raise ValueError("Invalid value for `gas`, must not be `None`")  # noqa: E501
        if gas is not None and gas < 0:  # noqa: E501
            raise ValueError("Invalid value for `gas`, must be a value greater than or equal to `0`")  # noqa: E501

        self._gas = gas

    @property
    def gas_price(self):
        """Gets the gas_price of this CreateContractData.  # noqa: E501

        Gas price  # noqa: E501

        :return: The gas_price of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._gas_price

    @gas_price.setter
    def gas_price(self, gas_price):
        """Sets the gas_price of this CreateContractData.

        Gas price  # noqa: E501

        :param gas_price: The gas_price of this CreateContractData.  # noqa: E501
        :type: int
        """
        if gas_price is None:
            raise ValueError("Invalid value for `gas_price`, must not be `None`")  # noqa: E501
        if gas_price is not None and gas_price < 0:  # noqa: E501
            raise ValueError("Invalid value for `gas_price`, must be a value greater than or equal to `0`")  # noqa: E501

        self._gas_price = gas_price

    @property
    def fee(self):
        """Gets the fee of this CreateContractData.  # noqa: E501

        Transaction fee  # noqa: E501

        :return: The fee of this CreateContractData.  # noqa: E501
        :rtype: int
        """
        return self._fee

    @fee.setter
    def fee(self, fee):
        """Sets the fee of this CreateContractData.

        Transaction fee  # noqa: E501

        :param fee: The fee of this CreateContractData.  # noqa: E501
        :type: int
        """
        if fee is None:
            raise ValueError("Invalid value for `fee`, must not be `None`")  # noqa: E501
        if fee is not None and fee < 0:  # noqa: E501
            raise ValueError("Invalid value for `fee`, must be a value greater than or equal to `0`")  # noqa: E501

        self._fee = fee

    @property
    def call_data(self):
        """Gets the call_data of this CreateContractData.  # noqa: E501

        Contract call data  # noqa: E501

        :return: The call_data of this CreateContractData.  # noqa: E501
        :rtype: str
        """
        return self._call_data

    @call_data.setter
    def call_data(self, call_data):
        """Sets the call_data of this CreateContractData.

        Contract call data  # noqa: E501

        :param call_data: The call_data of this CreateContractData.  # noqa: E501
        :type: str
        """
        if call_data is None:
            raise ValueError("Invalid value for `call_data`, must not be `None`")  # noqa: E501

        self._call_data = call_data

    def to_dict(self):
        """Returns the model properties as a dict"""
        result = {}

        for attr, _ in six.iteritems(self.swagger_types):
            value = getattr(self, attr)
            if isinstance(value, list):
                result[attr] = list(map(
                    lambda x: x.to_dict() if hasattr(x, "to_dict") else x,
                    value
                ))
            elif hasattr(value, "to_dict"):
                result[attr] = value.to_dict()
            elif isinstance(value, dict):
                result[attr] = dict(map(
                    lambda item: (item[0], item[1].to_dict())
                    if hasattr(item[1], "to_dict") else item,
                    value.items()
                ))
            else:
                result[attr] = value

        return result

    def to_str(self):
        """Returns the string representation of the model"""
        return pprint.pformat(self.to_dict())

    def __repr__(self):
        """For `print` and `pprint`"""
        return self.to_str()

    def __eq__(self, other):
        """Returns true if both objects are equal"""
        if not isinstance(other, CreateContractData):
            return False

        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """Returns true if both objects are not equal"""
        return not self == other
