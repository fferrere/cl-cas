# CL-CAS

Library to help [CAS authenticaton](https://apereo.github.io/cas/6.0.x/protocol/CAS-Protocol.html) to Common Lisp web applications.

Supports 2 modes of authentication (See CAS Protocol Specs) :
- Default : Single sign-on (SSO) mode 
- Renew : Force authentication (SSO Off)

Support :
- CAS Protocol V2 specs : Complete
- CAS Protocol V3 specs : Partially
  - Check an return serviceValide attributes 

This is a herlper library not a CAS Client.

## Installation
1. git clone https://github.com/fferrere/cl-cas
2. Add cl-cas system to your project (asd file) `:depends-on ("cl-cas" ...)`


## API

### URL Functions

#### Function cas-login-url

Returns CAS Server login URL

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Optional] service-url : URL of the Web app
- [Optional] renew : t or nil (default), t to force authentication (SSO off), nil to use SSO authentication
- [Optional] gateway : t or nil (default)

Example :
```
CL-USER> (cl-cas:cas-login-url "https://cas.example.org/cas" "http://www.example.org/service")

https://cas.example.org/cas/login?service=http%3A%2F%2Fwww.example.org%2Fservice
```

#### Function cas-service-validate-url

Returns CAS serviceValidate URL

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Required] service-url : URL of the Web app
- [Required] ticket : cas ticket (string)
- [Optional] pgt-url : Proxy Grating Ticket URL
- [Optional] renew : either nil (default) or t, t to force authentication (SSO off), nil to use SSO authentication
- [Optional] format : either :xml (défaut), or :json

Example :
```
CL-USER> (cl-cas:cas-logout-url "https://cas.example.org/cas" :url "http://www.example.org/logout")

https://cas.example.org/cas/logout?url=http%3A%2F%2Fwww.example.org%2Flogout
```

#### Function cas-proxy-validate-url

Returns CAS proxyValidate URL

- [Required] cas-server-url : URL of the Web CAS Server
- [Required] service-url : URL of the Web app
- [Required] ticket : cas ticket (string)
- [Optional] pgt-url : Proxy Grating Ticket URL
- [Optional] renew : either nil (default) or t, t to force authentication (SSO off), nil to use SSO authentication
- [Optional] format : either :xml (défaut), or :json

Example :
```
CL-USER> (cl-cas:cas-logout-url "https://cas.example.org/cas" :url "http://www.example.org/logout")

https://cas.example.org/cas/logout?url=http%3A%2F%2Fwww.example.org%2Flogout
```

#### Function cas-proxy-url

Returns CAS Proxy URL

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Required] target-service : URL of the Web app
- [Required] pgt : Proxy Grating Ticket (string)

Example :
```
CL-USER> (cl-cas:cas-logout-url "https://cas.example.org/cas" :url "http://www.example.org/logout")

https://cas.example.org/cas/logout?url=http%3A%2F%2Fwww.example.org%2Flogout
```

### Function cas-logout-url

Returns CAS Server logout URL.

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Optional] url : if specified, should be on the logout page with descriptive text (if supported by the CAS server).

Example :
```
CL-USER> (cl-cas:cas-logout-url "https://cas.example.org/cas" :url "http://www.example.org/logout")

https://cas.example.org/cas/logout?url=http%3A%2F%2Fwww.example.org%2Flogout
```

### Tickets Functions


#### Function cas-ticket

Extracts CAS ticket value from HTTP 'Query String'

Example :
HTTP URL : `https://cas.example.org/cas/validate?service=http%3A%2F%2Fwww.example.org%2Fservice&ticket=ST-1856339-aA5Yuvrxzpv8Tau1cYQ7`

```
CL-USER> (cl-cas:cas-ticket "service=http%3A%2F%2Fwww.example.org%2Fservice&ticket=ST-1856339-aA5Yuvrxzpv8Tau1cYQ7")

ST-1856339-aA5Yuvrxzpv8Tau1cYQ7
```

#### Function cas-service-validate

Checks the validity of a service ticket and returns :
- On ticket validation success : User Name, Proxy Grating Ticket, User Attributes (if returned by the server) .
- On ticket validation failure : nil.

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Required] service-url : URL of the Web app
- [Required] ticket : cas ticket (string)
- [Optional] pgt-url : Proxy Grating Ticket URL
- [Optional] renew : t or nil (default), t to force authentication (SSO off), nil to use SSO authentication
- [Optional] format : either :xml (défaut), or :json

```
CL-USER> (cl-cas:cas-service-validate "https://cas.example.org/cas" "http://www.example.org/service" "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7")

<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
    <cas:authenticationSuccess>
        <cas:user>username</cas:user>
        </cas:authenticationSuccess>
</cas:serviceResponse>
```

```
CL-USER> (cl-cas:cas-service-validate "https://cas.example.org/cas" "http://www.example.org/service" "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7" :format :json))

{
  "serviceResponse" : {
    "authenticationSuccess" : {
      "user" : "username",
      "attributes" : { }
    }
  }
}
```

#### Function cas-proxy-validate

Checks the validity of a service ticket and returns :
- On ticket validation success : user name, Proxy Grating Ticket, Proxies, user attributes (if returned by the server).
- On ticket validation failure : nil.

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Required] service-url : URL of the Web app
- [Required] ticket : cas ticket (string)
- [Optional] pgt-url : Proxy Grating Ticket URL
- [Optional] renew : t or nil (default), t to force authentication (SSO off), nil to use SSO authentication
- [Optional] format : either :xml (défaut), or :json

```
CL-USER> (cl-cas:cas-service-validate "https://cas.example.org/cas" "http://www.example.org/service" "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7")

<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
    <cas:authenticationSuccess>
        <cas:user>username</cas:user>
        </cas:authenticationSuccess>
</cas:serviceResponse>
```

#### Function cas-proxy

Checks the validity of a proxy grating ticket and returns :
- On Proxy Grating Ticket validation success : Proxy Ticket.
- On Proxy Grating Ticket validation failure : nil.

Parameters :
- [Required] cas-server-url : URL of the Web CAS Server
- [Required] target-service : URL of the Proxy Service
- [Required] pgt : Proxy Grating Ticket (string)

```
CL-USER> (cl-cas:cas-proxy "https://cas.example.org/cas" "http://www.service.com" "PGT-1856376-1HMgO86Z2ZKeByc5XdYD")

<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
    <cas:proxySuccess>
        <cas:proxyTicket>PT-1856392-b98xZrQN4p90ASrw96c8</cas:proxyTicket>
    </cas:proxySuccess>
</cas:serviceResponse>
```

### Conditions

#### Condition cas-url-invalid-parameters-error
           

## Demo

See https://github.com/fferrere/cas-demo

## References
- https://apereo.github.io/cas/6.0.x/protocol/CAS-Protocol.html
- https://apereo.github.io/cas/6.0.x/protocol/CAS-Protocol-V2-Specification.html

## Author

* Frédéric FERRERE (frederic.ferrere@gmail.com)

## Licence

Apache-2.0 (https://www.apache.org/licenses/LICENSE-2.0)


