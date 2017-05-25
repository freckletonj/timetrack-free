# timetrack-free

# Running

```bash
ssh-keygen -t rsa -b 4096 -N "" -f .secret/timetrack.key
```

# General Todo

## Errors
- return appropriate errors
- safe-exceptions
- ExceptT
- ServantError
- 

## Transactions
- http://stackoverflow.com/questions/31359894/catching-an-exception-from-rundb
- transaction errors log + return (exception-lifted?)
- runSqlConn(?) keeps things in a transaction
- transactionSave / transactionUndo?

## Auth
- access control
- credentials add appropriate dn?
- goog/facebook auth? <- lowest risk/compliance + easy for customers?
- sessions and tokens

--------------------------------------------------

# Tickets

last seen ticket # : 12


##############################

[ ] 6 - refactor, rearchitect, implement code quality stuff,
cyclomatic complexity?, stylish haskell, linter...

- try a cyclomatic complexity lib
- draw out dependencies
- restructure dirs/files/modules
- learn stylish and/or a linter, possibly implement into workflow

##############################


[X] 1 - user can log in through oauth and get a list of their repos

[X] 2 - user's oauth credentials are saved and api can use them ad lib
on user's behalf

[X] 3 - users sign in's can be persisted and authenticated through
sessions

- http://hackage.haskell.org/package/cryptonite-0.23/docs/Crypto-KDF-BCrypt.html

[ ] 4 - user workflows can be declared and tested
automatically. Perhaps just a simple requests lib, and actually run it
on IO. Fixtures are added each time a feature is added, and the db can
be torn down/remade each test, prob not too expensively.

[ ] 5 - move tickets to markdown, look into using an org-mode kanban
for task tracking? raw org-mode?

[ ] 7 - implement testing.

https://github.com/haskell-servant/servant-quickcheck?

[ ] 8 - figure out the right way to document stuff, perhaps with
motivation from Snoyman?

[ ] 9 - consider haskell propellor for config, docker, passwords, ssh
keys, build?, CI?, etc.

- https://abailly.github.io/posts/cm-infra-2.html
- https://propellor.branchable.com/haskell_newbie/

[ ] 10 - logging

[ ] 11 - users can only see things they're auth'd and auth'd for

[ ] 12 - wai-extra pretty printer


# Clean Code / Refactoring

- check
  - `-- TODO`
  - `error`
  - `return` -> `pure` ?

# Not to be Forgotten

- Persistent UUID

http://michaelxavier.net/posts/2015-04-14-Adding-a-UUID-Column-to-a-Persistent-Table.html

```haskell
import Data.UUID
UUID sqltype=uuid default=uuid_generate_v4()
```

- CRUD
https://github.com/nmdanny/perservant-loans/blob/master/src/Api/Crud.hs
