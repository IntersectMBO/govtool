# Handle a New Governance Action Type

:::note
Line references on this page point to commit `6522bd4` of the `develop` branch. Line numbers may have moved in newer commits.
:::

## Overview

This document describes the process of adding a new governance action type to the frontend application.

## Prerequisites

Every governance action should follow the [CIP-100](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0100) and [CIP-108](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0108) standards.

Person to contact: @mesudip

## Package

All the related changes are to be made under the `govtool/frontend` directory.

## Steps

### Type declarations

1. Add new `GovernanceActionType` enum property to [`govtool/frontend/src/types/governanceAction.ts`](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L4).
2. If the governance action requires a new field component - add it to the [`GovernanceActionField`](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L14) enum.
3. Create a new governance action field schema. Every governance action schema should extend from the [SharedGovernanceActionFieldSchema](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L31).
4. Add the new governance action schema below the [SharedGovernanceActionFieldSchema](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L31).
5. Add a new governance action schema to the union type of [GovernanceActionFieldSchemas](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L70).
6. Add the new type to the [GovernanceActionFields](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L78) record.
7. Add a builder for the new type in `CardanoProvider` ([`context/wallet.tsx`](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/context/wallet.tsx)) and wire it into [`useCreateGovernanceActionForm.ts`](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/hooks/forms/useCreateGovernanceActionForm.ts).
8. Update this page (`docs/docs/developers/operations/handle-new-governance-action-type.md`) with all the new declarations provided (eg.: line numbers, new types configurations).

### Fields declaration

1. Add new governance action field declaration to the [GOVERNANCE_ACTION_FIELDS](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/consts/governanceAction/fields.ts#L98) object.

### Custom validations

If a field needs custom validation, add a validator function under `src/utils/` (for example [`numberValidation`](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/utils/numberValidation.ts#L9)), export it from `src/utils/index.ts`, and reference it in the field's `rules.validate` in [`consts/governanceAction/fields.ts`](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/consts/governanceAction/fields.ts).

### Constants & Fields definitions

[GovernanceActionFieldSchemas](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L70) - includes all the governance action field schemas which are:

- component - the component which should be used to render the field (currently supporting are: 'Input', 'TextArea' and array of both of them).
- labelI18nKey - the i18n key for the field label.
- placeholderI18nKey - the i18n key for the field placeholder.
- tipI18nKey - the i18n key for the field tip.
- rules - the array of validation rules for the field [check rules property in react-hook-form](https://www.react-hook-form.com/api/usecontroller/controller/#:~:text=cleared%20value%20instead.-,rules,-Object).

[SharedGovernanceActionFieldSchema](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L31) - includes all the shared fields for the governance action - each field is of type [FieldSchema](https://github.com/IntersectMBO/govtool/blob/6522bd43ee52c5de9fe5f9ab0fb14f56858ff0ef/govtool/frontend/src/types/governanceAction.ts#L19) which corresponds to [CIP-108](https://github.com/cardano-foundation/CIPs/pull/632).

### How this works

Every governance action field is a part of the governance action schema.
The schema is used to render appropriate fields in a [form](https://github.com/IntersectMBO/govtool/blob/develop/govtool/frontend/src/components/organisms/CreateGovernanceActionSteps/CreateGovernanceActionForm.tsx).
Logic behind validation and hashing is also based on the schema and is handled in the [useCreateGovernanceActionForm](https://github.com/IntersectMBO/govtool/blob/develop/govtool/frontend/src/hooks/forms/useCreateGovernanceActionForm.ts).

Defining the new Governance action type in [Type declarations](#type-declarations) will allow the application to handle the new governance action type.

Defining the new Governance action field in [Fields declaration](#fields-declaration) will allow the application to render the new governance action field.

They both are used in the [CreateGovernanceActionForm](https://github.com/IntersectMBO/govtool/blob/develop/govtool/frontend/src/components/organisms/CreateGovernanceActionSteps/CreateGovernanceActionForm.tsx) component.

### Testing

After defining a new governance action type and field, it is important to test the new governance action type and field in the [CreateGovernanceActionForm](https://github.com/IntersectMBO/govtool/blob/develop/govtool/frontend/src/components/organisms/CreateGovernanceActionSteps/CreateGovernanceActionForm.tsx) component.

On the application it is approachable under `/create_governance_action` route.
