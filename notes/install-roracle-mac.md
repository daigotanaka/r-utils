Download:
- instantclient-basic-macos.x64-12.1.0.2.0
- instantclient-sqlplus-macos.x64-12.1.0.2.0
- instantclient-sdk-macos.x64-12.1.0.2.0
from http://www.oracle.com/technetwork/topics/intel-macsoft-096467.html
You cannot wget on this. You need to create an account with Oracle.

```
brew tap InstantClientTap/instantclient
brew install instantclient-basic
brew install instantclient-sqlplus
brew install instantclient-sdk
```

```
wget https://cran.r-project.org/src/contrib/ROracle_1.3-1.tar.gz
```

Make sure you installed DBI on R.

```
export LD_LIBRARY_PATH=/usr/local/Cellar/instantclient-basiclite/12.1.0.2.0/lib:$LD_LIBRARY_PATH
R CMD INSTALL --configure-args='--with-oci-lib=/usr/local/Cellar/instantclient-basic/12.1.0.2.0/lib --with-oci-inc=/usr/local/Cellar/instantclient-sdk/12.1.0.2.0/lib/sdk/include' ROracle_1.3-1.tar.gz
R```
