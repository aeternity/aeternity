import nacl.encoding
import nacl.signing
from nacl.signing import SigningKey, VerifyKey
from nacl.hash import blake2b

import common

def new_private():
    return SigningKey.generate()

def public_key(private_key):
    return private_key.verify_key

def address(public_key):
    return common.encode_pubkey(public_key.encode(encoder=nacl.encoding.RawEncoder))

def sign(message, private_key):
    return private_key.sign(message).signature

def sign_encode_tx(packed_tx, private_key):
    signature = sign(packed_tx, private_key)
    signed_encoded = common.encode_signed_tx(packed_tx, [bytearray(signature)])
    return signed_encoded

def sign_verify_encode_tx(packed_tx, private_key, public_key):
    signature = sign(packed_tx, private_key)
    assert verify(signature, packed_tx, public_key)
    signed_encoded = common.encode_signed_tx(packed_tx, [bytearray(signature)])
    return signed_encoded

def tx_hash_from_signed_encoded(signed_encoded):
    signed = common.base58_decode(signed_encoded);
    return common.encode_tx_hash(blake2b(data=signed, digest_size=32, encoder=nacl.encoding.RawEncoder))

def verify(signature, message, public_key):
    return public_key.verify(message, signature)

