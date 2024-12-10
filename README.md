# Flop #

Stores network links in a text-formatted database; provides CLI snippets of
configured links.

The compilation requires [rebar3](https://rebar3.org/docs/getting-started/).

## Database operations
You may start the server `flop` from an existing database:
```
$ make run
> flop:load("foo").
#{id => "3f0156fcd3e8078a834e9aa9f6dc3f3c",name => "qEOd",
  file => "foo",'@' => "2023-08-23T21:29:48+02:00"}
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
> flop:read(#{log=>enable}).
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
#{id => "0147815aa96a7ecddf1722ad0a0c803a",name => "qEOd",
  status => ok,file => "foo",
  '@' => "2023-08-23T21:34:47+02:00"}
```
## CLI snippets
Using the database, it produces CLI snippets. To date, these Cisco Nexus CLI are implemented: 
* `description/1`
* `intf_eth/1`
* `intf_vlan/1`
* `intf_prt_chnl/1`
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
