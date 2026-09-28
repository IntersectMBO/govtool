# How Governance Action Vote Totals are Calculated in GovTool

## DRep Vote Total Equation

GovTool aims to show vote total values which help the user gauge likelihood of ratification of an action.

### Intro definitions

* Active DRep stake = all the voting power of DReps within the active state&#x20;
  * (This does not include any in-active DReps OR retired DReps)
* Total Active Stake = Active DRep stake + auto no confidence stake
  * We do not include any abstain within this, as they are not part of ratification equation

### For a governance action

#### :thought\_balloon: Abstain Total&#x20;

* Total voting power of DRep Abstain votes + auto-abstain stake

#### :white\_check\_mark: Yes Total

* IF governance action type != 'NoConfidence'&#x20;
  * Total of voting power of DReps Yes votes&#x20;
* IF governance action type == 'NoConfidence'
  * Total of voting power of DReps Yes votes + auto no confidence stake

#### :white\_check\_mark: Yes Percentage

* (Yes Total / Total Active Stake) x 100

#### :x: No Total

* IF governance action type != 'NoConfidence'
  * Total of voting power of DReps No votes + auto no confidence stake
* IF governance action type == 'NoConfidence'
  * Total of voting power of DReps No votes

#### :x: No Percentage

* (No Total / Total Active Stake) x 100

#### :ballot\_box: Not Voted Total (remainder of Total Active Stake)

* Total Active Stake - Yes Total - No Total

#### :ballot\_box:  Not Voted Percentage (remainder of Total Active Stake Percentage)

* 100 - yes percentage - no percentage
  * this should equal ((Total Active Stake - Yes Total - No Total) / Total Active Stake ) \* 100

## DRep Vote Total Implementation

[See GovTool's SQL query](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql) run on DB-Sync. This pulls all the proposal data, and vote totals.

* GovTool takes DRep voting power from [`drep_distr` table of DB-Sync](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md#drep_distr) for the registered DReps and the "predefined voting option DReps", this data is only updated once per epoch.
* GovTool takes the newest vote from DReps for that governance action, filtering out votes from DReps who have recently retired.
* In the SQL query
  * [Gets the latest epoch of DRep distribution](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql#L1-L7)
  * [Gets the voting power of the predefined no confidence DRep](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql#L18-L26)
  * [Gets the voting power of the predefined always abstain DRep](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql#L18-L26)
  * [Calculates Yes Total](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql#L242-L247)
  * [Calculates No Total](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql#L248-L253)
  * [Calculates Abstain Total](https://github.com/IntersectMBO/govtool/blob/develop/govtool/backend/sql/list-proposals.sql#L254)

### Example

* So we have some DRep John Doe, who voted `yes` on X governance action
* John Doe has 100k Voting power coming from his own stake and all delegators (we are taking that value directly from db-sync (no calculations on GovTool side))
* We have another DRep - Andre, who has 200k Voting power!
* Andre also likes the governance action and votes `yes`
* As a result - we display the sum of their voting powers, so it would be 300k for `yes` votes
* Important note - John Doe and Andre can change their votes

## SPO Vote Totals

* For SPOs logic is completely the same as the DReps one - the difference is that we are taking the voting power from a different table - dedicated to SPOs (`pool_stat`)
