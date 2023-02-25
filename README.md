# Flop #

Stores network links as a database; provides CLI snippets of configured links.

## Database operations
You may start the server from an existing database, or an empty one:
```
> flop:start("foo").
> flop:read().
#{file => "foo",
  links =>
      [#{from =>
             #{addr => "77:b8:55:e6:3f:b4",dev => "W7hNXuBr",port => 125},
         id => "315d044abb58db3551bd6953a2ea420430e9d0ec",
         net => "171.175.170.98/24",tag => "s7vr",
         to =>
             #{addr => "52:c4:75:2b:ed:78",dev => "zVSGXHf4",port => 89},
         vlan => 166}],
  name => "JtBP"}
```
From a template, you may create a link:
```
> Link = flop:template_link().
#{from =>
      #{addr => "2d:ca:3a:48:11:96",dev => "OH9YFoFD",port => 43},
  net => "12.210.239.42/24",tag => "gwTp",
  to =>
      #{addr => "b5:13:6a:9b:e8:2b",dev => "4fxT36vE",port => 20},
  vlan => 3451}

> flop:create(Link).
#{db =>
      #{file => "foo",
        links =>
            [#{from =>
                   #{addr => "77:b8:55:e6:3f:b4",dev => "W7hNXuBr",port => 125},
               id => "315d044abb58db3551bd6953a2ea420430e9d0ec",
               net => "171.175.170.98/24",tag => "s7vr",
               to =>
                   #{addr => "52:c4:75:2b:ed:78",dev => "zVSGXHf4",port => 89},
               vlan => 166},
             #{from =>
                   #{addr => "2d:ca:3a:48:11:96",dev => "OH9YFoFD",port => 43},
               id => "67bd0e177728f6e9246ab9ec7fd921f8c41f7871",
               net => "12.210.239.42/24",tag => "gwTp",
               to =>
                   #{addr => "b5:13:6a:9b:e8:2b",dev => "4fxT36vE",port => 20},
               vlan => 3451}],
        name => "JtBP"},
  status => ok}
```
Update a link (using its ID)
```
> flop:update("315", tag, "bar").
#{db =>
      #{file => "foo",
        links =>
            [#{from =>
                   #{addr => "2d:ca:3a:48:11:96",dev => "OH9YFoFD",port => 43},
               id => "67bd0e177728f6e9246ab9ec7fd921f8c41f7871",
               net => "12.210.239.42/24",tag => "gwTp",
               to =>
                   #{addr => "b5:13:6a:9b:e8:2b",dev => "4fxT36vE",port => 20},
               vlan => 3451},
             #{from =>
                   #{addr => "77:b8:55:e6:3f:b4",dev => "W7hNXuBr",port => 125},
               id => "315d044abb58db3551bd6953a2ea420430e9d0ec",
               log =>
                   [#{tag => "s7vr",until => "2023-02-25T14:35:50+01:00"}],
               net => "171.175.170.98/24",tag => "bar",
               to =>
                   #{addr => "52:c4:75:2b:ed:78",dev => "zVSGXHf4",port => 89},
               vlan => 166}],
        name => "JtBP"},
  status => ok}
```
Delete a link
```
> flop:delete("67b").
#{db =>
      #{file => "foo",
        links =>
            [#{from =>
                   #{addr => "77:b8:55:e6:3f:b4",dev => "W7hNXuBr",port => 125},
               id => "315d044abb58db3551bd6953a2ea420430e9d0ec",
               log =>
                   [#{tag => "s7vr",until => "2023-02-25T14:35:50+01:00"}],
               net => "171.175.170.98/24",tag => "bar",
               to =>
                   #{addr => "52:c4:75:2b:ed:78",dev => "zVSGXHf4",port => 89},
               vlan => 166}],
        name => "JtBP"},
  status => ok}
```
And save the database
```
> flop:save().
#{db =>
      #{'@' => "2023-02-25T14:37:58+01:00",file => "foo",
        id => "65906485474a9d928d5a2e715a83f4b83381479b",
        links =>
            [#{from =>
                   #{addr => "77:b8:55:e6:3f:b4",dev => "W7hNXuBr",port => 125},
               id => "315d044abb58db3551bd6953a2ea420430e9d0ec",
               log =>
                   [#{tag => "s7vr",until => "2023-02-25T14:35:50+01:00"}],
               net => "171.175.170.98/24",tag => "bar",
               to =>
                   #{addr => "52:c4:75:2b:ed:78",dev => "zVSGXHf4",port => 89},
               vlan => 166}],
        name => "JtBP"},
  status => ok}
```
## CLI snippets
Using the database, it produces CLI snippets. To date, these Cisco Nexus CLI are implemented: 
* `flop:description/1`
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
