use serde_json::{Map, Value};
use serde::{Serialize, Deserialize};
use std::collections::{BTreeMap, HashMap};
use schemars::JsonSchema;
use std::hash::{Hash, DefaultHasher, Hasher};
use crate::TransactionFields;

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq)]
pub enum OpParamTypes {
    Address,
    String,
    Tuple,
    FixedArray(usize),
    Array,
    FixedBytes(usize),
    Bool,
    Uint,
    Int,
    BigUint,
    BigInt,
    GiantUint,
    GiantInt,
    Mapping,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq)]
pub enum OpParams {
    Address([u8; 20]),
    String(String),
    Tuple(Vec<OpParams>),
    FixedArray(Vec<OpParams>, usize),
    Array(Vec<OpParams>),
    FixedBytes(Vec<u8>, usize),
    Bool(bool),
    Byte(u8),
    Uint(u32),
    Int(i32),
    BigUint([u64; 4]),
    BigInt([i64; 4]),
    GiantUint([u64; 8]),
    GiantInt([i64; 8]),
    Mapping(HashMap<String, OpParams>)
}

impl Ord for OpParams {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hashed_self = hasher.finish() as u128;
        let mut hasher = DefaultHasher::new();
        other.hash(&mut hasher);
        let hashed_other = hasher.finish() as u128;
        hashed_self.cmp(&hashed_other)
    }
}

impl PartialOrd for OpParams {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for OpParams {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            OpParams::Address(addr) => addr.hash(state),
            OpParams::String(str) => str.hash(state),
            OpParams::Tuple(tup) => tup.hash(state),
            OpParams::FixedArray(arr, size) => { 
                arr.hash(state);
                size.hash(state);
            },
            OpParams::Array(arr) => arr.hash(state),
            OpParams::FixedBytes(arr, size) => {
                arr.hash(state);
                size.hash(state);
            },
            OpParams::Bool(b) => b.hash(state),
            OpParams::Byte(b) => b.hash(state),
            OpParams::Uint(n) => n.hash(state),
            OpParams::Int(i) => i.hash(state),
            OpParams::BigUint(n) => n.hash(state),
            OpParams::BigInt(i) => i.hash(state),
            OpParams::GiantUint(n) => n.hash(state),
            OpParams::GiantInt(i) => i.hash(state),
            OpParams::Mapping(map) => {
                let sorted: BTreeMap<&String, &OpParams> = map.iter().collect();
                sorted.hash(state);
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct ProgramSchema {
    pub contract: Contract
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct Contract {
    pub name: String,
    pub version: String,
    pub language: String,
    pub ops: HashMap<String, Ops>
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct Ops {
    pub description: String,
    pub help: Option<String>,
    pub signature: Option<OpSignature>,
    pub required: Option<Vec<Required>> 
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct OpSignature {
    #[serde(flatten)]
    pub op_signature: HashMap<String, String>,
    pub params_mapping: HashMap<String, ParamSource>,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct ParamSource {
    pub source: String,
    pub field: Option<String>,
    pub key: Option<String>,
    pub position: usize
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", content = "details")]
pub enum Required {
    Call(CallMap),
    Read(ReadMap),
    Lock(LockPair),
    Unlock(LockPair),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct CallMap {
    calling_program: TransactionFields,
    original_caller: TransactionFields,
    program_id: String, 
    op: String,
    inputs: String, 
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct ReadMap {
    items: Vec<(String, String, String)>, 
    contract_blobs: Option<Vec<String>> 
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct LockPair {
    account: String,
    token: String
}

impl ProgramSchema {
    pub fn contract(&self) -> &Contract {
        &self.contract
    }

    pub fn name(&self) -> String {
        self.contract().name.clone()
    }

    pub fn version(&self) -> String {
        self.contract().version.clone()
    }

    pub fn language(&self) -> String {
        self.contract().language.clone()
    }

    pub fn ops(&self) -> &HashMap<String, Ops> {
        &self.contract().ops
    }

    pub fn get_op(&self, name: &str) -> Option<&Ops> {
        self.ops().get(name)
    }

    pub fn get_prerequisites(&self, op: &str) -> std::io::Result<Vec<Required>> {
        let (_key, value) = self.contract.ops.get_key_value(op).ok_or(
            std::io::Error::new(std::io::ErrorKind::Other, "Invalid `op`: Not defined in schema")
        )?;
        
        if let Some(reqs) = &value.required {
            return Ok(reqs.clone())
        }

        Ok(Vec::new())
    }

    #[allow(unused)]
    pub(crate) fn parse_op_inputs(&self, op: &str, json_inputs: &str) -> std::io::Result<()> {
        let (_key, value) = self.contract.ops.get_key_value(op).ok_or(
            std::io::Error::new(std::io::ErrorKind::Other, "Invalid `op`: Not defined in schema")
        )?;
        
        let mut json: Map<String, Value> = serde_json::from_str(json_inputs)?;

        if let Some(function_signature) = &value.signature {
            for (k, v) in &function_signature.params_mapping {
                dbg!(&k, &v);
            }
        }

        Ok(())
    }
}

pub trait Parameterize {
    fn into_op_params(self) -> Vec<OpParams>;
}

pub trait Parameter {
    type Err;

    fn from_op_param(op_param: OpParams) -> Result<Self, Self::Err>
        where Self: Sized;

    fn into_op_param(self) -> OpParams;
}
