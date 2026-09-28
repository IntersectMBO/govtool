# How Governance Action Vote Totals are Calculated in GovTool

GovTool aims to show vote total values which help the user gauge likelihood of ratification of an action. Vote totals are computed for live Governance Actions (not yet ratified, enacted, expired or dropped).

The raw totals come from GovTool's backend, which runs SQL queries on DB-Sync. The percentages and "Not Voted" values are then calculated in the GovTool frontend.

## DRep Vote Total Equation

### Intro definitions

* Active DRep stake = all the voting power of DReps within the active state&#x20;
  * This does not include retired DReps, or DReps that have been inactive for longer than the `drepActivity` protocol parameter (no vote or registration/update within that many epochs)
* Total Active Stake = Active DRep stake + auto no confidence stake − DRep Abstain votes cast on this action
  * Auto-abstain stake is never included, as it is not part of the ratification equation
  * Explicit Abstain votes on the action are removed as well, for the same reason

### For a governance action

#### :thought\_balloon: Abstain Total&#x20;

* Total voting power of DRep Abstain votes + auto-abstain stake
  * The DRep Abstain votes come from the SQL query; the auto-abstain stake is added by the frontend
  * No percentage is shown for Abstain

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

* Total Active Stake − explicit DRep Yes votes − explicit DRep No votes
  * The auto no confidence stake is therefore counted as "Not Voted" in this value

#### :ballot\_box:  Not Voted Percentage (remainder of Total Active Stake Percentage)

* 100 − Yes Percentage − No Percentage
  * Unlike the Not Voted value, this treats the auto no confidence stake as part of Yes/No

## DRep Vote Total Implementation

[See GovTool's SQL query](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql) run on DB-Sync. This pulls all the proposal data, and vote totals. The denominator comes from a [separate query](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/get-network-total-stake.sql), and the percentages are calculated in the [frontend](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/components/molecules/VotesSubmitted.tsx#L75-L111).

* GovTool takes DRep voting power from [`drep_distr` table of DB-Sync](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md#drep_distr) for the registered DReps and the "predefined voting option DReps", this data is only updated once per epoch.
* GovTool takes the newest vote from DReps for that governance action, filtering out votes from DReps who have recently retired.
* In the SQL query
  * [Filters to live governance actions](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L1-L9)
  * [Gets the latest epoch](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L18-L27)
  * [Gets the voting power of the predefined no confidence DRep](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L30)
  * [Gets the voting power of the predefined always abstain DRep](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L31)
  * [Calculates Yes Total](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L338-L343)
  * [Calculates No Total](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L344-L349)
  * [Calculates DRep Abstain votes](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/list-proposals.sql#L350)
  * [Calculates the active DRep stake and auto no confidence stake](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/backend/sql/get-network-total-stake.sql#L56-L89)

The same queries are mirrored in the TypeScript backend (`govtool/backend-ts/sql`).

### Example

* So we have some DRep John Doe, who voted `yes` on X governance action
* John Doe has 100k Voting power coming from his own stake and all delegators (we are taking that value directly from db-sync (no calculations on GovTool side))
* We have another DRep - Andre, who has 200k Voting power!
* Andre also likes the governance action and votes `yes`
* As a result - we display the sum of their voting powers, so it would be 300k for `yes` votes
* Important note - John Doe and Andre can change their votes

## SPO Vote Totals

* SPO totals use the latest vote per pool, weighted by `pool_stat.voting_power` for the current epoch.
* Percentages are divided by the total pool stake (`pool_stat.stake`) for the current epoch.
* Unlike DReps, no auto-abstain or auto no confidence adjustment is applied, and Abstain votes are not removed from the denominator.
* Not Voted value = total pool stake − Yes − No − Abstain; Not Voted Percentage = 100 − Yes Percentage − No Percentage.

## Constitutional Committee Vote Totals

* CC totals are a count of each committee member's latest vote (one member, one vote).
* Percentages are divided by the number of committee members.

## Thresholds

* The DRep and SPO thresholds shown are the voting-threshold protocol parameters that apply to the Governance Action type, read from the current epoch parameters.
* The CC threshold shown is the committee quorum (numerator/denominator). No CC threshold is shown for Info actions.

## Which totals are shown

With full governance (protocol version 10 and later):

* DRep totals are always shown.
* SPO totals are not shown for New Constitution, Treasury Withdrawal, and Protocol Parameter Changes that don't touch security-group parameters, since SPOs don't vote on these.
* CC totals are not shown for Motion of No Confidence and Update Committee actions, since the CC doesn't vote on these.
