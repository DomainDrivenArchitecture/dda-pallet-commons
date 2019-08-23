# dda-pallet-commons
[![Clojars Project](https://img.shields.io/clojars/v/dda/dda-pallet-commons.svg)](https://clojars.org/dda/dda-pallet-commons)
[![Build Status](https://travis-ci.org/DomainDrivenArchitecture/dda-pallet-commons.svg?branch=master)](https://travis-ci.org/DomainDrivenArchitecture/dda-pallet-commons)

[![Slack](https://img.shields.io/badge/chat-clojurians-green.svg?style=flat)](https://clojurians.slack.com/messages/#dda-pallet/) | [<img src="https://meissa-gmbh.de/img/community/Mastodon_Logotype.svg" width=20 alt="team@social.meissa-gmbh.de"> team@social.meissa-gmbh.de](https://social.meissa-gmbh.de/@team) | [Website & Blog](https://domaindrivenarchitecture.org)

Common utils for dda-pallet

## compatability
dda-pallet is compatible to the following versions
 * pallet 0.9
 * clojure 1.9

## Features
* Encrypted Credential Managament
* Utilities for testing pallet plans
* ServerTest Framework to implement integrtion tests for servers

## Use encrypted Credentials

You can encrypt
```
(ns ...
	(:require
  		[dda.pallet.commons.encrypted-credentials :as crypto]))

(crypto/encrypt
  (crypto/get-public-key
    {:user-home "/home/user/"
     :key-id "-key-id-"})
    {:account "acnt"
     :secret "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"})
```


or decrypt with
```
(def encrypted-secret
	{:account "acnt",
	 :secret "-----BEGIN PGP MESSAGE-----....-----END PGP MESSAGE-----\n"})

(crypto/decrypt
 (crypto/get-secret-key {:user-home "/home/mje/"
                         :key-id key-id})
   encrypted-secret
   key-passphrase)
```


See also: https://domaindrivenarchitecture.org/posts/2016-10-24-encrypted-credentials/

## Use ServerTest
We typically use remote-whitebox-local-tested method for running server-tests:
1. Collect facts from remote target systems: We connected by ssh and execute some small bash and provide the result returned.
2. Parse local: We parse the returned results on CI system.
3. Test local: We run tests on the parsed information on CI system.

Example for collecting facts - e.g. in settings phase:

```
(package-fact/collect-packages-fact)
```

Execute tests - e.g. in test phase:
```
(package-test/test-installed? "atom")
```

For complete example see:
https://github.com/DomainDrivenArchitecture/dda-managed-vm/blob/master/src/org/domaindrivenarchitecture/pallet/crate/managed_vm.clj or
https://github.com/DomainDrivenArchitecture/dda-managed-ide/blob/master/src/org/domaindrivenarchitecture/pallet/crate/managed_ide.clj

# Build & Contribute
## Setup project with eclipse
In order to do some work on your own, you can add [lein-idefiles "0.2.1"] to your lein plugins and execute

```bash
git clone git@github.com:DomainDrivenArchitecture/dda-pallet-commons.git
lein idefiles eclipse
```

up to now you're ready to start working ...


## License

Copyright © 2015, 2016, 2017, 2018 meissa GmbH
Licensed under the [Apache License, Version 2.0](LICENSE) (the "License")
Pls. find licenses of our subcomponents [here](doc/SUBCOMPONENT_LICENSE)
