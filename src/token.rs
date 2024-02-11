use hex::FromHexError;
use serde::{Serialize, Deserialize, Serializer, Deserializer, de::Visitor};
use ethereum_types::U256 as EthU256;
use std::collections::BTreeMap;
use std::fmt::{Display, Debug};
use std::ops::{AddAssign, SubAssign};
use schemars::JsonSchema;
use uint::construct_uint;

use crate::{Address, RecoverableSignature, Transaction};

pub const TOKEN_WITNESS_VERSION: &'static str = "0.1.0";

construct_uint! {
	/// 256-bit unsigned integer.
    #[derive(JsonSchema)] 
    #[serde(rename_all = "camelCase")]
	pub struct U256(4);
}

impl Serialize for U256 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let hex_str = format!("0x{:064x}", self);
        serializer.serialize_str(&hex_str)
    }
}

impl<'de> Deserialize<'de> for U256 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(U256Visitor)
    }
}

struct U256Visitor;

impl<'de> Visitor<'de> for U256Visitor {
    type Value = U256;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a 64-character hex string or [u64; 4] array as a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<U256, E>
    where
        E: serde::de::Error,
    {

        let value = if v.starts_with("0x") { &v[2..] } else { v };
        if value.starts_with("[") && v.ends_with("]") {
            // Parse as a byte array
            let nums_str = &v[1..v.len() - 1];
            let nums: Vec<u64> = nums_str.split(',')
                .map(str::trim)
                .map(|s| s.parse::<u64>().map_err(E::custom))
                .collect::<Result<Vec<u64>, E>>()?;

            if nums.len() == 4 {
                let arr = [nums[0], nums[1], nums[2], nums[3]];
                Ok(U256(arr))
            } else {
                Err(E::custom("Array does not have 4 elements"))
            }
        } else if value.len() == 64 {
            // Parse as a hex string
            log::info!("{}", &value);
            let decoded = hex::decode(value).map_err(E::custom)?;
            if decoded.len() == 32 {
                let mut bytes = [0u8; 32];
                bytes.copy_from_slice(&decoded[..]);
                Ok(U256::from(&bytes))
            } else {
                Err(E::custom("decoded result is improper length"))
            }
        } else {
            Err(E::custom("Invalid format for U256"))
        }
    }
}

impl From<EthU256> for &mut U256 {
    fn from(value: EthU256) -> Self {
        value.into()
    }
}

impl From<EthU256> for &U256 {
    fn from(value: EthU256) -> Self {
        value.into()
    }
}

impl From<&mut U256> for U256 {
    fn from(value: &mut U256) -> Self {
        value.clone()
    }
}

impl From<&mut U256> for EthU256 {
    fn from(value: &mut U256) -> Self {
        EthU256(value.0.clone())
    }
}

impl From<U256> for EthU256 {
    fn from(value: U256) -> Self {
        EthU256(value.0)
    }
}

impl From<EthU256> for U256 {
    fn from(value: EthU256) -> Self {
        U256(value.0)
    }
}

impl From<&U256> for EthU256 {
    fn from(value: &U256) -> Self {
        EthU256(value.0.clone())
    }
}

impl From<&EthU256> for U256 {
    fn from(value: &EthU256) -> Self {
        U256(value.0.clone())
    }
}


/// Represents a generic data container.
///
/// This structure is used to store arbitrary data as a vector of bytes (`Vec<u8>`).
/// It provides a default, cloneable, serializable, and debuggable interface. It is
/// typically used for storing data that doesn't have a fixed format or structure.
#[derive(Clone, Default, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub struct ArbitraryData(BTreeMap<String, String>);

impl Display for ArbitraryData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0)
    }
}

impl ArbitraryData {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn insert(&mut self, key: String, value: String) {
        self.0.insert(key, value);
    }

    pub fn remove(&mut self, key: &str) -> Option<String> {
        self.0.remove(key)
    }
    
    pub fn extend(&mut self, iter: BTreeMap<String, String>) {
        self.0.extend(iter);
    }

    pub fn inner(&self) -> &BTreeMap<String, String> {
        &self.0
    }

    pub fn inner_mut(&mut self) -> &mut BTreeMap<String, String> {
        &mut self.0
    }

    pub fn to_hex(&self) -> Result<String, serde_json::Error> {
        let bytes = self.to_bytes()?;
        Ok(hex::encode(&bytes))
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, serde_json::Error> {
        serde_json::to_vec(&self)
    }

    pub fn from_hex(hex: &str) -> Result<Self, FromHexError> {
        Ok(serde_json::from_slice(&hex::decode(hex)?).map_err(|_| {
            FromHexError::InvalidStringLength
        }))?
    }
}

/// Represents metadata as a byte vector.
///
/// This structure is designed to encapsulate metadata, stored as a vector of bytes.
/// It supports cloning, serialization, and debugging. The metadata can be of any
/// form that fits into a byte array, making it a flexible container.
#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub struct Metadata(BTreeMap<String, String>);

impl Metadata {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn inner_mut(&mut self) -> &mut BTreeMap<String, String> {
        &mut self.0
    }

    pub fn inner(&self) -> &BTreeMap<String, String> {
        &self.0
    }

    pub fn to_hex(&self) -> Result<String, Box<bincode::ErrorKind>> {
        let bytes = self.to_bytes()?;
        Ok(hex::encode(&bytes))
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, Box<bincode::ErrorKind>> {
        bincode::serialize(&self)
    }

    pub fn from_hex(hex: &str) -> Result<Self, FromHexError> {
        Ok(bincode::deserialize(&hex::decode(hex)?).map_err(|_| {
            FromHexError::InvalidStringLength
        }))?
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub enum TokenType {
    Fungible,
    NonFungible,
    Data
}

#[derive(Builder, Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub struct Token {
    program_id: Address,
    owner_id: Address,
    balance: U256,
    metadata: Metadata,
    token_ids: Vec<U256>,
    allowance: BTreeMap<Address, U256>,
    approvals: BTreeMap<Address, Vec<U256>>,
    data: ArbitraryData,
    status: Status,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum TokenField {
    ProgramId,
    OwnerId,
    Balance,
    Metadata,
    TokenIds,
    Allowance,
    Approvals,
    Data,
    Status,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum TokenFieldValue {
    Balance(BalanceValue),
    Metadata(MetadataValue),
    TokenIds(TokenIdValue),
    Allowance(AllowanceValue),
    Approvals(ApprovalsValue),
    Data(DataValue),
    Status(StatusValue),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum BalanceValue {
    Credit(U256),
    Debit(U256),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum MetadataValue {
    Insert(String, String),
    Extend(BTreeMap<String, String>),
    Remove(String),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum TokenIdValue {
    Push(U256),
    Extend(Vec<U256>),
    Insert(usize, U256),
    Pop,
    Remove(U256),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum AllowanceValue {
    Insert(Address, U256),
    Extend(Vec<(Address, U256)>),
    Remove(Address, U256),
    Revoke(Address),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum ApprovalsValue {
    Insert(Address, Vec<U256>),
    Extend(Vec<(Address, Vec<U256>)>),
    Remove(Address, Vec<U256>),
    Revoke(Address),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum DataValue {
    Insert(String, String),
    Extend(BTreeMap<String, String>),
    Remove(String),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum StatusValue {
    Reverse,
    Lock,
    Unlock,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)] 
#[serde(rename_all = "camelCase")]
pub enum Status {
    Locked,
    Free,
}

impl AddAssign for Token {
    fn add_assign(&mut self, rhs: Self) {
        let new_balance = EthU256::from(self.balance) + EthU256::from(rhs.balance);
        self.balance = new_balance.into();
    }
}

impl SubAssign for Token {
    fn sub_assign(&mut self, rhs: Self) {
        let new_balance: EthU256 = EthU256::from(self.balance) - EthU256::from(rhs.balance);
        self.balance = new_balance.into();
    }
}

#[derive(Builder, Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct TokenWitness {
    user: Address,
    token: Address,
    init: Token,
    transactions: TransactionGraph,
    finalized: Box<Token>,
    sig: RecoverableSignature,
    version: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct TransactionGraph {
    transactions: BTreeMap<[u8; 32], GraphEntry>
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct GraphEntry {
    transaction: Transaction,
    dependencies: Vec<[u8; 32]> 
}
