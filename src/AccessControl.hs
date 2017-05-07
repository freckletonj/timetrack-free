module AccessControl where

{-

ABAC - Attribute Based Access Control

Services ----------

1. PEP - Policy Enforcement Point - generates an authorization req
2. PDP - Policy Decision Point - evaluate requests against policies, return Permit/Deny


Attributes ----------

1. Subject
2. Action
3. Resource
4. Context

-}

permitted :: Bool
permitted = True
