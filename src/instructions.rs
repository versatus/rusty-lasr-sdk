use std::hash::Hash;
use crate::{Address, TokenField, Transaction,TokenFieldValue,ProgramField, ProgramFieldValue, DataValue};
use schemars::JsonSchema;
use serde::{Serialize, Deserialize};
use crate::AddressOrNamespace;

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

impl CreateInstruction {
    pub fn accounts_involved(&self) -> Vec<AddressOrNamespace> {
        let mut accounts_involved = vec![
            self.program_namespace.clone(),
            self.program_id.clone(),
            AddressOrNamespace::Address(self.program_owner.clone()),
        ];

        for dist in &self.distribution {
            accounts_involved.push(dist.to.clone());
        }

        accounts_involved
    }

    pub fn program_namespace(&self) -> &AddressOrNamespace {
        &self.program_namespace
    }

    pub fn program_id(&self) -> &AddressOrNamespace {
        &self.program_id
    }

    pub fn program_owner(&self) -> &Address {
        &self.program_owner
    }

    pub fn total_supply(&self) -> &crate::U256 {
        &self.total_supply
    }

    pub fn initialized_supply(&self) -> &crate::U256 {
        &self.initialized_supply
    }

    pub(crate) fn distribution(&self) -> &Vec<TokenDistribution> {
        &self.distribution
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

impl TokenDistribution { 
    pub fn program_id(&self) -> &AddressOrNamespace {
        &self.program_id
    }

    pub fn to(&self) -> &AddressOrNamespace {
        &self.to
    }

    pub fn amount(&self) -> &Option<crate::U256> {
        &self.amount
    }

    pub fn token_ids(&self) -> &Vec<crate::U256> {
        &self.token_ids
    }

    pub fn update_fields(&self) -> &Vec<TokenUpdateField> {
        &self.update_fields
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

impl TokenUpdateField {
    pub fn field(&self) -> &TokenField {
        &self.field
    } 

    pub(crate) fn value(&self) -> &TokenFieldValue {
        &self.value
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "camelCase")]
pub struct ProgramUpdateField {
    field: ProgramField,
    value: ProgramFieldValue 
}

impl ProgramUpdateField {
    pub fn new(field: ProgramField, value: ProgramFieldValue) -> Self {
        Self { field, value }
    }
    
    pub fn field(&self) -> &ProgramField {
        &self.field
    }

    pub fn value(&self) -> &ProgramFieldValue {
        &self.value
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct UpdateInstruction {
    updates: Vec<TokenOrProgramUpdate>
}

impl Default for UpdateInstruction {
    fn default() -> Self {
        UpdateInstruction { updates: vec![TokenOrProgramUpdate::default()] }
    }
}

impl UpdateInstruction {
    pub fn new(updates: Vec<TokenOrProgramUpdate>) -> Self {
        Self { updates }
    }

    pub(crate) fn accounts_involved(&self) -> Vec<AddressOrNamespace> {
        let mut accounts_involved = Vec::new();
        for update in &self.updates {
            match update {
                TokenOrProgramUpdate::TokenUpdate(token_update) => accounts_involved.push(token_update.account.clone()),
                TokenOrProgramUpdate::ProgramUpdate(program_update) => accounts_involved.push(program_update.account.clone()),
            }
        }
        accounts_involved
    }

    pub fn updates(&self) -> &Vec<TokenOrProgramUpdate> {
        &self.updates
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

impl TokenUpdate {
    pub fn account(&self) -> &AddressOrNamespace {
        &self.account
    }

    pub fn token(&self) -> &AddressOrNamespace {
        &self.token
    }

    pub fn updates(&self) -> &Vec<TokenUpdateField> {
        &self.updates
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ProgramUpdate {
    account: AddressOrNamespace,
    updates: Vec<ProgramUpdateField>
}

impl ProgramUpdate {
    pub fn new(account: AddressOrNamespace, updates: Vec<ProgramUpdateField>) -> Self {
        Self { account, updates }
    }

    pub fn account(&self) -> &AddressOrNamespace {
        &self.account
    }

    pub fn updates(&self) -> &Vec<ProgramUpdateField> {
        &self.updates
    }
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

impl TransferInstruction {
    pub fn new(
        token: Address,
        from: AddressOrNamespace,
        to: AddressOrNamespace,
        amount: Option<crate::U256>,
        ids: Vec<crate::U256>
    ) -> Self {
        Self { token, from, to, amount, ids }
    }

    pub(crate) fn accounts_involved(&self) -> Vec<AddressOrNamespace> {
        vec![self.from.clone(), self.to.clone()]
    }

    pub fn token(&self) -> &Address {
        &self.token
    }

    pub fn from(&self) -> &AddressOrNamespace {
        &self.from
    }

    pub fn to(&self) -> &AddressOrNamespace {
        &self.to
    }

    pub fn amount(&self) -> &Option<crate::U256> {
        &self.amount
    }

    pub fn ids(&self) -> &Vec<crate::U256> {
        &self.ids
    }

    pub fn replace_this_with_to(&mut self, transaction: &Transaction, _this: &AddressOrNamespace, field: &str) -> Result<(), std::io::Error> {
        match field {
            "from" => {
                self.from = AddressOrNamespace::Address(transaction.to());
            }
            "to" => {
                self.to = AddressOrNamespace::Address(transaction.to());
            }
            _ => {
                return Err(
                    std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "received an invalid string when calling `replace_this_with_to`"
                    )
                )
            }
        }
        Ok(())
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

impl BurnInstruction {
    pub(crate) fn accounts_involved(&self) -> Vec<AddressOrNamespace> {
        vec![self.from.clone()]
    }

    pub fn caller(&self) -> &Address {
        &self.caller
    }

    pub fn program_id(&self) -> &AddressOrNamespace {
        &self.program_id
    }
    
    pub fn token(&self) -> &Address {
        &self.token
    }

    pub fn from(&self) -> &AddressOrNamespace {
        &self.from
    }

    pub fn amount(&self) -> &Option<crate::U256> {
        &self.amount
    }

    pub fn token_ids(&self) -> &Vec<crate::U256> {
        &self.token_ids
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

impl Instruction {
    pub fn get_accounts_involved(&self) -> Vec<AddressOrNamespace> {
        match self {
            Self::Create(create) => create.accounts_involved(),
            Self::Update(update) => update.accounts_involved(),
            Self::Transfer(transfer) => transfer.accounts_involved(),
            Self::Burn(burn) => burn.accounts_involved(),
            Self::Log(_log) => vec![] 
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub enum ContractLogType {
    Info(String),
    Error(String),
    Warn(String),
    Debug(String)
}
