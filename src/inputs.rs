use crate::{Address, TokenField, Transaction,Account};
use schemars::JsonSchema;
use serde::{Serialize, Deserialize};
use crate::OpParams;


/// This file contains types the protocol uses to prepare data, structure it 
/// and call out to a particular compute payload.

/// The inputs type for a contract call, this is built from a combination of 
/// transaction data and pre-requisite data the protocol acquires in accordance
/// with the developers program schema (WARNING: PROGRAM SCHEMAS ARE 
/// EXPERIMENTAL AND NOT YET ENABLED) Inputs takes a protocol populated  
/// compute agent `version` which is a 32 bit signed integer, an optional 
/// `Account` for the contract's account under the field `account_info`, a 
/// `Transaction` under the `transaction` field and then an `op`, i.e. an 
/// operation that will be called from *within* the contract, and the `inputs`
/// to that `op`. The `inputs` to an op are always a JSON string, it can be 
/// an empty JSON string, and sometimes, developers may choose to use additional
/// data that is provided in the `Transaction`. the `Inputs` struct is 
/// serialized into JSON when passed into the contract, and can be deserialized
/// with either JSON helper functions and/or custom JSON parsing. The developer
/// has the flexibility to do with the `Inputs`, represented by JSON as they
/// choose. 
#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename(serialize = "computeInputs", deserialize = "computeInputs"), rename_all = "camelCase")]
pub struct Inputs {
    /// The compute agent version
    pub version: i32,
    /// An optional program/contract's account in the protocol
    pub account_info: Option<Account>,
    /// The transaction that made the original call
    pub transaction: Transaction,
    /// The operation in the program being called
    pub op: String,
    /// The inputs to the contract operation being called
    #[serde(rename(serialize = "contractInputs", deserialize = "contractInputs"))]
    pub inputs: String,
}

/// The pre-requisite instructions for a contract call
///
/// In many instances a contract will need to read data from an account, or 
/// have another contract called and return data that will then be used by 
/// the contract being called by the user. As a result, the protocol needs to 
/// be able to somehow know what contracts to call before calling this program
/// and what data to read, and where to put that data in the `inputs` field in 
/// the `Inputs` struct, i.e. what the key should be and what the value
/// should be. This will all be determined by the program schema. Program 
/// schema is a developer defined schema that informs the protocol of information
/// (WARNING: PROGRAM SCHEMAS ARE EXPERIMENTAL AND NOT YET ENABLED)
///
// TODO(asmith): Replace outputs with a proper type, instead of a vector of 
// tuples
#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)] 
pub struct ParamPreRequisite {
    pre_requisites: PreRequisite,
    outputs: Vec<(usize, OpParams)>,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub enum PreRequisite {
    Call(CallParams),
    Unlock(AddressPair),
    Read(ReadParams), 
    Lock(AddressPair),
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct CallParams {
    pub calling_program: Address,
    pub original_caller: Address,
    pub program_id: Address,
    pub inputs: Inputs,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddressPair {
    pub account_address: Address,
    pub token_address: Address,
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct ReadParams {
    pub items: Vec<(Address, Address, TokenField)>,
    pub contract_blobs: Vec<Address>,
}
