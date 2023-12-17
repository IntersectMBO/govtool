
user = person "ADA Holder" "😀"

userDRep = person "dRep" "😎"
userCCMember = person "CC Member" "🧐"
userSPO = person "SPO" "🤠"

userGovActSub = person "Gov Action Submitter" "😛"

browser = softwareSystem "Browser" "Firefox, Chrome, Safari, Edge" "Browser"

cardanoWallet = softwareSystem "Cardano Wallet" "" "Owned by Other Entities"

hwWallet = softwareSystem "HW Wallet" "Cardano Hardware Wallet" "Owned by Other Entities"

group "CardanoWorld" {
    cardanoNode = softwareSystem "CardanoNode"
    cardanoCLI = softwareSystem "cardano-cli Tool"
    cardanoCLI -> cardanoNode "uses"
}

group "Somewhere?" {
    metadataServer = softwareSystem "dRep/Governance Action Metadata Server" "Off chain metadata storage used to fetch metadata from dRep registrations + governance actions that happen on chain" ""
}

# group "Community Tooling" {
#     communityFE = softwareSystem

# }


group "Owned by Gov Analysis Squad"{
    dAppFrontEnd = softwareSystem "Voltaire dApp Frontend" "Web App" "" {
        walletConnector = container "walletConnector" "" "" ""
        httpClient = container "HTTP Client" "" "" ""

        walletConnector -> cardanoWallet "(dAPP CONNECTOR CIP) \nPOST /drep-reg/ \nPOST /vote\nPOST /drep-ret/\n GET /stake-key/ "

    }

    dAppBackEnd = softwareSystem "Voltaire dApp Backend" "HTTP Service in front of a chain follower and DB" {

        database = container "Database" "" "Some Database" "Database"
        voltaireAPI = container "Voltaire DB API" "REST API that offers the ability for clients to find Voltaire related chain data" ""
        chainFollower = container "Chain Follower" "Follows the Cardano chain, reduces data, sinks it a store" "txpipe/oura"
        txValidationService = container "Validation Service" "Consumes ordered events read from the sink, validates the transactions and when valid stores in store" ""
        kafka = container "Message Broker" "Message / Event broker" "apache/kafka" "Message Broker"

        chainFollower -> cardanoNode "follows"
        chainFollower -> chainFollower "applyFilter(rawBlock)"
        chainFollower -> kafka "sink filtered dapp registration"
        kafka -> txValidationService "consumes dapp registrations"
        txValidationService -> metadataServer "GET /gov-act-meta"
        txValidationService -> database "stores dapp registrations"
        voltaireAPI -> database "reads"

        dAppFrontEnd.httpClient -> voltaireAPI "GET /dreps \nGET /gov-act"

        dAppFrontEnd.httpClient -> metadataServer "GET /gov-metadata"
    }
}

user -> browser "uses"
userDRep -> browser "uses"
userDRep -> metadataServer "POST /drep-meta/"
userSPO -> cardanoCLI "uses"
userCCMember -> cardanoCLI "uses"

userGovActSub -> metadataServer "POST /gov-act-meta/"

# userGovActSub -> cardanoCLI "POST /gov-act/"

browser -> dAppFrontEnd "connects"

// Light wallet and HW wallet
hwWallet -> cardanoWallet "integrates"

// User's browser attaches to GVC FE
browser -> dAppFrontEnd "connects"

