workspace "Voltaire Implementation Draft" {
    !docs ./documentation
    !adrs ./decisions
    !identifiers hierarchical
    
    model {
        !include dapp.dsl 
    }

    views {
        systemLandscape all {
            title "Volatire Tech Implementation System Overview [Draft]"
            include *
            autoLayout lr
        }

        systemContext dAppFrontEnd {
            include *
            autoLayout lr 
        }

        systemContext dAppBackEnd {
            include *
            autoLayout lr 
        }
        
        container dAppBackEnd {
            include *
            autolayout lr
        }

        container dAppFrontEnd {
            include *
            autolayout lr
        }

        // Colour pallette: https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=4
        styles {
            element "Software System" {
                background #0570b0
                color #ffffff
                shape RoundedBox
            }

            element "Container" {
                background #74a9cf
                color #ffffff
                shape RoundedBox
            }

            element "Component" {
                background #bdc9e1
                color #ffffff
                shape RoundedBox
            }

            element "Person" {
                background #66c2a5
                color #ffffff
                shape person
            }

            element "Owned by Other Entities" {
                background #999999
                color #ffffff
            }

            element "Browser" {
                background #999999
                color #ffffff
                shape WebBrowser
            }

            element "Database" {
                shape Cylinder
            }               
        }

        branding {
            logo data:image/png;base64,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
            # font 
        }        
    }
}