# Flop #

Stores network links as a database; provides CLI snippets of configured links.

## Database operations
The application provides two servers: `contract_checker` & `flop`. The server `flop` acts as a database. You may start the server `flop` from an existing database:
```
> application:start(flop).
ok

> flop:load("foo").
#{links =>
      [#{id => "86685741066227ad30072f1c0959c5ed",tag => "X3yb",
         net => "245.121.74.254/24",vlan => 480,
         to =>
             #{port => 40,addr => "58:95:14:9d:ab:ff",dev => "3uK3vJUE"},
         from =>
             #{port => 82,addr => "12:8a:4b:48:df:15",dev => "iitwtUF7"}}],
  name => "2O0a",file => foo}
```
From a template, you may create a link:
```
> Link = flop:template_link().
#{tag => "MFDU",net => "223.0.42.113/24",vlan => 400,
  to =>
      #{port => 215,addr => "da:de:cb:1b:16:4b",dev => "f7OCJjBW"},
  from =>
      #{port => 221,addr => "c2:3b:ae:88:c6:55",dev => "H1SS0dRK"}}

> flop:create(Link).
#{links =>
      #{id => "6df9b6cec6ffdd5979ec5e407d06957f",tag => "MFDU",
        net => "223.0.42.113/24",vlan => 400,
        to =>
            #{port => 215,addr => "da:de:cb:1b:16:4b",dev => "f7OCJjBW"},
        from =>
            #{port => 221,addr => "c2:3b:ae:88:c6:55",dev => "H1SS0dRK"}},
  status => ok}
```
Update a link (using its ID)
```
> flop:update("6d", tag, "bar").
#{links =>
      #{id => "6df9b6cec6ffdd5979ec5e407d06957f",tag => "bar",
        log =>
            [#{tag => "MFDU",until => "2023-07-29T19:27:12+02:00"}],
        net => "223.0.42.113/24",vlan => 400,
        to =>
            #{port => 215,addr => "da:de:cb:1b:16:4b",dev => "f7OCJjBW"},
        from =>
            #{port => 221,addr => "c2:3b:ae:88:c6:55",dev => "H1SS0dRK"}},
  status => ok}
```
Delete a link
```
> flop:read().
#{status => ok,
  db =>
      #{links =>
            [#{id => "86685741066227ad30072f1c0959c5ed",tag => "X3yb",
               net => "245.121.74.254/24",vlan => 480,
               to =>
                   #{port => 40,addr => "58:95:14:9d:ab:ff",dev => "3uK3vJUE"},
               from =>
                   #{port => 82,addr => "12:8a:4b:48:df:15",dev => "iitwtUF7"}},
             #{id => "6df9b6cec6ffdd5979ec5e407d06957f",tag => "bar",
               log =>
                   [#{tag => "MFDU",until => "2023-07-29T19:27:12+02:00"}],
               net => "223.0.42.113/24",vlan => 400,
               to =>
                   #{port => 215,addr => "da:de:cb:1b:16:4b",dev => "f7OCJjBW"},
               from =>
                   #{port => 221,addr => "c2:3b:ae:88:c6:55",dev => "H1SS0dRK"}}],
        name => "2O0a",file => foo}}

> flop:delete("86").
#{links =>
      #{id => "86685741066227ad30072f1c0959c5ed",tag => "X3yb",
        net => "245.121.74.254/24",vlan => 480,
        to =>
            #{port => 40,addr => "58:95:14:9d:ab:ff",dev => "3uK3vJUE"},
        from =>
            #{port => 82,addr => "12:8a:4b:48:df:15",dev => "iitwtUF7"}},
  status => ok}
```
And save the database
```
> flop:save().
#{status => ok,
  db =>
      #{id => "bc8cee06364712a481145bad443279a3",
        links =>
            [#{id => "6df9b6cec6ffdd5979ec5e407d06957f",tag => "bar",
               log =>
                   [#{tag => "MFDU",until => "2023-07-29T19:27:12+02:00"}],
               net => "223.0.42.113/24",vlan => 400,
               to =>
                   #{port => 215,addr => "da:de:cb:1b:16:4b",dev => "f7OCJjBW"},
               from =>
                   #{port => 221,addr => "c2:3b:ae:88:c6:55",dev => "H1SS0dRK"}}],
        name => "2O0a",file => foo,
        '@' => "2023-07-29T19:30:00+02:00"}}
```
## CLI snippets
Using the database, it produces CLI snippets. To date, these Cisco Nexus CLI are implemented: 
* `description/1`
* `intf_eth/1`
* `intf_vlan/1`
* `route_map/1`
* `vlan/1`

The functions expect the atom `nxos`.

`flop:print/1` prints out the snippets:
```
> flop:print(flop:intf_eth(nxos)).
interface ethernet 1/125
     description cx=JtBP;to=zVSGXHf4_89
     switchport mode trunk
     switchport trunk allowed vlan 166
```