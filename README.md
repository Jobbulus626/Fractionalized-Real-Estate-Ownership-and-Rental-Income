# Fractionalized Real Estate Smart Contract

A Clarity smart contract for managing fractionalized real estate ownership and rental income distribution on the Stacks blockchain.

## Features

- Create tokenized real estate properties
- Mint ownership shares as NFTs
- Manage rental agreements
- Automated rental income distribution

## Contract Functions

### Property Management
- `create-property`: Create a new property with specified shares and rental price
- `mint-share`: Purchase an ownership share of a property
- `rent-property`: Rent a property for a specified duration
- `distribute-rental-income`: Distribute rental income to shareholders

### Read-Only Functions
- `get-property`: Get property details
- `get-shares-owned`: Get number of shares owned by an address
- `get-share-owner`: Get the owner of a specific share

## Usage

1. Deploy the contract using Clarinet
2. Create a property using `create-property`
3. Users can mint shares using `mint-share`
4. Tenants can rent properties using `rent-property`
5. Contract owner distributes rental income using `distribute-rental-income`

## Requirements

- Clarinet
    - Stacks blockchain wallet
    ```

