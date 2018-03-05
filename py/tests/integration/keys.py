import ecdsa
from ecdsa import SigningKey, VerifyingKey
from ecdsa.der import remove_sequence
from hashlib import sha256

import common

def new_private():
    return SigningKey.generate(curve=ecdsa.SECP256k1, hashfunc=sha256) 

def private_from_bytearray(arr):
    return SigningKey.from_string(arr, curve=ecdsa.SECP256k1, hashfunc=sha256) 

def public_key(private_key):
    return private_key.get_verifying_key()

def public_from_bytearray(arr):
    return VerifyingKey.from_string(arr, curve=ecdsa.SECP256k1, hashfunc=sha256) 

def address(public_key):
    arr = bytearray([0x04] + map(ord, list(public_key.to_string())))
    return common.encode_pubkey(str(arr))

def sign(message, private_key):
    return private_key.sign(message, sigencode=ecdsa.util.sigencode_der)

def sign_encode_tx(msgpacked_tx, private_key):
    unpacked_tx = common.unpack_tx(msgpacked_tx)
    signature = sign(msgpacked_tx, private_key)
    signed_encoded = common.encode_signed_tx(unpacked_tx, [bytearray(signature)]) 
    return signed_encoded

def sign_verify_encode_tx(msgpacked_tx, unpacked_tx, private_key, public_key):
    signature = sign(msgpacked_tx, private_key)
    assert verify(signature, msgpacked_tx, public_key)
    signed_encoded = common.encode_signed_tx(unpacked_tx, [bytearray(signature)]) 
    return signed_encoded

def verify(signature, message, public_key):
    return public_key.verify(signature, message, sigdecode=ecdsa.util.sigdecode_der)

def to_string(key):
    return str(map(ord, list(key.to_string())))
