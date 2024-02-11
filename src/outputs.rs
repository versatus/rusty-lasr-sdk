use crate::{Instruction, Inputs, Transaction, Certificate, TokenWitness};
use schemars::JsonSchema;
use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Outputs {
    #[serde(rename(serialize = "computeInputs", deserialize = "computeInputs"), alias="inputs")]
    inputs: Inputs,
    instructions: Vec<Instruction>,
}

impl Outputs {
    pub fn new(inputs: Inputs, instructions: Vec<Instruction>) -> Self {
        Self { inputs, instructions }
    }
    
    pub fn instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }
}

/// This type is constructed from the combination of the original transaction,
/// the constructed inputs, all outputs from contract call, and any 
/// pre-requisite contract call, witnesses, and an optional certificate
/// if the transaction results have been certified.
#[derive(Clone, Debug, Serialize, Deserialize, JsonSchema)]
pub struct CallTransactionResult {
    transaction: Transaction,
    inputs: Inputs,
    outputs: Vec<Outputs>,
    certificate: Option<Certificate>,
    witnesses: Vec<TokenWitness>
}
