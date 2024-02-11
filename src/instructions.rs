use std::hash::Hash;
use crate::{Address, TokenField, TokenFieldValue,ProgramField, ProgramFieldValue, DataValue};
use schemars::JsonSchema;
use serde::{Serialize, Deserialize};
use crate::AddressOrNamespace;

//TODO(asmith): Implement custom builders for each instruction type
#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct CreateInstruction {
    program_namespace: AddressOrNamespace,
    program_id: AddressOrNamespace,
    program_owner: Address,
    total_supply: crate::U256,
    initialized_supply: crate::U256,
    distribution: Vec<TokenDistribution>
}

impl Default for CreateInstruction {
    fn default() -> Self {
        CreateInstruction { 
            program_namespace: AddressOrNamespace::This, 
            program_id: AddressOrNamespace::Address(Address::from([0; 20])), 
            program_owner: Address::from([0; 20]), 
            total_supply: crate::U256::from(0), 
            initialized_supply: crate::U256::from(0), 
            distribution: vec![TokenDistribution::default()] 
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct TokenDistribution {
    program_id: AddressOrNamespace,
    to: AddressOrNamespace,
    amount: Option<crate::U256>,
    token_ids: Vec<crate::U256>,
    update_fields: Vec<TokenUpdateField>
}

impl Default for TokenDistribution {
    fn default() -> Self {
        TokenDistribution { 
            program_id: AddressOrNamespace::This, 
            to: AddressOrNamespace::Address(Address::from([0; 20])), 
            amount: Some(crate::U256::from(0)), 
            token_ids: vec![crate::U256::from(0)], 
            update_fields: vec![TokenUpdateField::default()] 
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub enum TokenOrProgramUpdateField {
    TokenUpdateField(TokenUpdateField),
    ProgramUpdateField(ProgramUpdateField)
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub enum TokenOrProgramUpdate {
    TokenUpdate(TokenUpdate),
    ProgramUpdate(ProgramUpdate),
}

impl Default for TokenOrProgramUpdate {
    fn default() -> Self {
        TokenOrProgramUpdate::TokenUpdate(TokenUpdate::default())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct TokenUpdateField {
    field: TokenField,
    value: TokenFieldValue
}

impl Default for TokenUpdateField {
    fn default() -> Self {
        TokenUpdateField { 
            field: TokenField::Data,
            value: TokenFieldValue::Data(DataValue::Insert("some".to_string(), "data".to_string())) 
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct ProgramUpdateField {
    field: ProgramField,
    value: ProgramFieldValue 
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct UpdateInstruction {
    updates: Vec<TokenOrProgramUpdate>
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct UpdateInstructionBuilder {
    updates: Vec<TokenOrProgramUpdate>
}

impl UpdateInstructionBuilder {
    pub fn new() -> Self {
        Self {
            updates: Vec::new()
        }
    }

    pub fn add(mut self, update: TokenOrProgramUpdate) -> Self {
        self.updates.push(update);
        self
    }
}

impl Default for UpdateInstructionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for UpdateInstruction {
    fn default() -> Self {
        UpdateInstruction { updates: vec![TokenOrProgramUpdate::default()] }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TokenUpdate {
    account: AddressOrNamespace,
    token: AddressOrNamespace,
    updates: Vec<TokenUpdateField>
}

impl Default for TokenUpdate {
    fn default() -> Self {
        TokenUpdate { 
            account: AddressOrNamespace::Address(Address::from([0; 20])),
            token: AddressOrNamespace::This, 
            updates: vec![TokenUpdateField::default()]
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ProgramUpdate {
    account: AddressOrNamespace,
    updates: Vec<ProgramUpdateField>
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct TransferInstruction {
    token: Address,
    from: AddressOrNamespace,
    to: AddressOrNamespace,
    amount: Option<crate::U256>,
    ids: Vec<crate::U256>
}


impl Default for TransferInstruction {
    fn default() -> Self {
        TransferInstruction { 
            token: Address::from([0; 20]), 
            from: AddressOrNamespace::This, 
            to: AddressOrNamespace::Address(Address::from([0; 20])), 
            amount: Some(crate::U256::from(0)), 
            ids: vec![crate::U256::from(0)] 
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct BurnInstruction {
    caller: Address,
    program_id: AddressOrNamespace,
    token: Address,
    from: AddressOrNamespace,
    amount: Option<crate::U256>,
    token_ids: Vec<crate::U256>
}

impl Default for BurnInstruction {
    fn default() -> Self {
        BurnInstruction { 
            caller: Address::from([0; 20]),
            program_id: AddressOrNamespace::This,
            token: Address::from([0; 20]),
            from: AddressOrNamespace::Address(Address::from([0; 20])),
            amount: Some(crate::U256::from(0)),
            token_ids: vec![crate::U256::from(0)] 
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct LogInstruction(pub ContractLogType);

/// An enum representing the instructions that a program can return 
/// to the protocol. Also represent types that tell the protocol what  
/// the pre-requisites of a given function call are.
/// All enabled languages have equivalent types
#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub enum Instruction {
    /// The return type created by the construction method of a contract 
    Create(CreateInstruction),
    /// Tells the protocol to update a field, should almost never be used  
    /// to add balance to a token or add a token id (for Non-fungible or Data tokens)
    /// should prrimarily be used to update approvals, allowances, metadata, arbitrary data
    /// etc. Transfer or burn should be used to add/subtract balance. Lock/Unlock should be used 
    /// to lock value
    Update(UpdateInstruction),
    /// Tells the protocol to subtract balance of one address/token pair and add to different
    /// address 
    Transfer(TransferInstruction), 
    /// Tells the protocol to burn a token (amount or id for NFT/Data tokens)
    Burn(BurnInstruction),
    /// Tells the protocol to log something
    Log(LogInstruction) 
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub enum ContractLogType {
    Info(String),
    Error(String),
    Warn(String),
    Debug(String)
}
