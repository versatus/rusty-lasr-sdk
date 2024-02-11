use std::{collections::{BTreeMap, BTreeSet}, hash::Hash, fmt::{Debug, LowerHex, Display}, str::FromStr};
use hex::{FromHexError, ToHex};
use schemars::JsonSchema;
use serde::{Serialize, Deserialize, Deserializer, Serializer};
use serde::de::Visitor;
use secp256k1::PublicKey;
use sha3::{Digest, Keccak256};
use crate::{
    Token,
    ArbitraryData,
    Metadata, 
    MetadataValue, 
    DataValue
};
use derive_builder::Builder;

#[derive(Clone, Debug, thiserror::Error)]
pub struct AccountCacheError;

impl Display for AccountCacheError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}


pub type AccountResult<T> = Result<T, Box<dyn std::error::Error + Send>>;

impl Serialize for Address {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> 
    where
        S: Serializer
    {
        let hex_string = hex::encode(self.inner());
        serializer.serialize_str(&format!("0x{}", hex_string))
    }
}

struct AddressVisitor;

impl<'de> Visitor<'de> for AddressVisitor {
    type Value = Address;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an address in either hex string or byte array format")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value.starts_with("0x") {
            let bytes = hex::decode(&value[2..]).map_err(E::custom)?;
            if bytes.len() == 20 {
                let mut arr = [0u8; 20];
                arr.copy_from_slice(&bytes);
                Ok(Address(arr))
            } else {
                Err(E::custom("Hex string does not represent a valid Address"))
            }
        } else if value.starts_with("[") && value.ends_with("]") {
            let bytes_str = &value[1..value.len() - 1];
            let bytes: Vec<u8> = bytes_str.split(',')
                .map(str::trim)
                .map(|s| s.parse::<u8>().map_err(E::custom))
                .collect::<Result<Vec<u8>, E>>()?;

            if bytes.len() == 20 {
                let mut arr = [0u8; 20];
                arr.copy_from_slice(&bytes);
                Ok(Address(arr))
            } else {
                Err(E::custom("invalid length for address"))
            }
        } else {
            Err(E::custom("Invalid address format"))
        }
    }
}

impl<'de> Deserialize<'de> for Address {
    fn deserialize<D>(deserializer: D) -> Result<Address, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(AddressVisitor)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum AddressOrNamespace {
    Address(Address),
    Namespace(Namespace),
    This,
}

/// Represents a 20-byte Ethereum Compatible address.
/// 
/// This structure is used to store Ethereum Compatible addresses, which are 
/// derived from the public key. It implements traits like Clone, Copy, Debug,
/// Serialize, Deserialize, etc., for ease of use across various contexts.
#[derive(Clone, Copy, Debug, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub struct Address([u8; 20]);

impl Address {
    /// Creates a new address from a 20 byte array
    pub fn new(bytes: [u8; 20]) -> Address {
        Address(bytes)
    }

    /// Converts the inner Address to a full hexadecimal string
    /// this exists because in the Disply implementation we abbreviate the 
    /// address
    pub fn to_full_string(&self) -> String {
        format!("0x{:x}", self)
    }

    pub fn from_hex(hex_str: &str) -> Result<Self, FromHexError> {
        let hex_str = if hex_str.starts_with("0x") { &hex_str[2..] } else { hex_str };
        let bytes = hex::decode(hex_str)?;
        let mut addr_inner = [0u8; 20];
        if bytes.len() != 20 {
            return Err(FromHexError::OddLength)
        }

        addr_inner.copy_from_slice(&bytes[..]);
        return Ok(Address(addr_inner));
    }

    pub fn inner(&self) -> [u8; 20] {
        self.0.clone()
    }
}

/// Represents a 32-byte account hash.
///
/// This structure is used to store current state hash associated with an account
// It supports standard traits for easy handling and
/// comparison operations.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct AccountHash([u8; 32]);

impl AccountHash {
    /// Creates a new `AccountHash` instance from a 32-byte array.
    ///
    /// This constructor is used to instantiate an `AccountHash` object with a given hash.
    pub fn new(hash: [u8; 32]) -> Self {
        Self(hash)
    }
}

/// This is currently not used
#[derive(Builder, Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub struct AccountNonce {
    bridge_nonce: crate::U256,
    send_nonce: crate::U256,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct ProgramNamespace(Namespace, Address);

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct Namespace(pub String);

impl FromStr for Namespace {
    type Err = Box<dyn std::error::Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let string = s.to_string(); 
        Ok(Self(string))
    }
}

impl From<String> for Namespace {
    fn from(value: String) -> Self {
        Self(value.clone())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum ProgramField {
    LinkedPrograms,
    Metadata,
    Data,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum ProgramFieldValue {
    LinkedPrograms(LinkedProgramsValue),
    Metadata(MetadataValue),
    Data(DataValue),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum LinkedProgramsValue {
    Insert(Address),
    Extend(Vec<Address>),
    Remove(Address),
}

#[derive(Builder, Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct ProgramAccount {
    namespace: Namespace,
    linked_programs: BTreeMap<Address, Token>,
    //TODO(asmith): Store Metadata in the Namespace
    metadata: Metadata,
    data: ArbitraryData,
}

impl ProgramAccount {
    pub fn new(
        namespace: Namespace,
        linked_programs: Option<BTreeMap<Address, Token>>,
        metadata: Option<Metadata>,
        data: Option<ArbitraryData>
    ) -> Self {
        let linked_programs = if let Some(p) = linked_programs {
            p.clone()
        } else {
            BTreeMap::new()
        };

        let metadata = if let Some(m) = metadata {
            m.clone()
        } else {
            Metadata::new()
        };

        let data = if let Some(d) = data {
            d.clone()
        } else {
            ArbitraryData::new()
        };

        Self { namespace, linked_programs, metadata, data }
    }

    pub fn namespace(&self) -> Namespace {
        self.namespace.clone()
    }

    pub fn linked_programs(&self) -> BTreeMap<Address, Token> {
        self.linked_programs.clone()
    }

    pub fn metadata(&self) -> Metadata {
        self.metadata.clone()
    }

    pub fn data(&self) -> ArbitraryData {
        self.data.clone()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub enum AccountType {
    User,
    Program(Address),
}

/// Represents an LASR account.
///
/// This structure contains details of an LASR account, including its address, associated
/// programs, nonce, signatures, hashes, and certificates. It implements traits for
/// serialization, hashing, and comparison.
#[derive(Builder, Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub struct Account {
    account_type: AccountType,
    program_namespace: Option<AddressOrNamespace>,
    owner_address: Address,
    programs: BTreeMap<Address, Token>,
    nonce: crate::U256,
    program_account_data: ArbitraryData,
    program_account_metadata: Metadata,
    program_account_linked_programs: BTreeSet<AddressOrNamespace>,
}

impl Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hex_str: String = self.encode_hex();
        write!(f, "0x{}...{}", &hex_str[0..4], &hex_str[hex_str.len() - 4..])
    }
}

impl From<[u8; 20]> for Address {
    fn from(value: [u8; 20]) -> Self {
        Address(value)
    }
}

impl From<&[u8; 20]> for Address {
    fn from(value: &[u8; 20]) -> Self {
        Address(*value)
    }
}


impl FromStr for Address {
    type Err = FromHexError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let hex_str = if s.starts_with("0x") {
            &s[2..]
        } else {
            s
        };

        if hex_str == "0" {
            return Ok(Address::new([0u8; 20]))
        }

        if hex_str == "1" {
            let mut inner: [u8; 20] = [0; 20];
            inner[19] = 1;
            return Ok(Address::new(inner))
        }

        let decoded = hex::decode(hex_str)?;
        if decoded.len() != 20 {
            return Err(FromHexError::InvalidStringLength);
        }

        let mut inner: [u8; 20] = [0; 20];
        inner.copy_from_slice(&decoded);
        Ok(Address::new(inner))
    }
}

impl AsRef<[u8]> for Address {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl From<Address> for [u8; 20] {
    fn from(value: Address) -> Self {
        value.0
    }
}

impl From<&Address> for [u8; 20] {
    fn from(value: &Address) -> Self {
        value.0.to_owned()
    }
}

impl From<Address> for ethereum_types::H160 {
    fn from(value: Address) -> Self {
        ethereum_types::H160(value.0)
    }
}

impl LowerHex for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for byte in self.0 {
            write!(f, "{:02x}", byte)?;
        }
        Ok(())
    }
}
impl From<ethereum_types::H160> for Address {
    fn from(value: ethereum_types::H160) -> Self {
        Address::new(value.0)
    }
}

impl From<PublicKey> for Address {
    /// Converts a `PublicKey` into an `Address`.
    ///
    /// This function takes a public key, serializes it, and then performs Keccak256
    /// hashing to derive the Ethereum address. It returns the last 20 bytes of the hash
    /// as the address.
    fn from(value: PublicKey) -> Self {
        let serialized_pk = value.serialize_uncompressed();

        let mut hasher = Keccak256::new();

        hasher.update(&serialized_pk[1..]);

        let result = hasher.finalize();
        let address_bytes = &result[result.len() - 20..];
        let mut address = [0u8; 20];
        address.copy_from_slice(address_bytes);

        Address(address)
    }
}

impl From<[u8; 32]> for Address {
    fn from(value: [u8; 32]) -> Self {
        let mut hasher = Keccak256::new();

        hasher.update(&value[0..]);

        let result = hasher.finalize();
        let address_bytes = &result[result.len() - 20..];
        let mut address = [0u8; 20];
        address.copy_from_slice(address_bytes);

        Address(address)
    }
}
