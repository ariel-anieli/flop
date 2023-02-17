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
             #{addr => "6e:bb:ee:b9:4e:89",dev => "QtaXf3du",port => 142},
         id => "2152e93a832bc38bc08d0209ecf0e600d03d718a",
         net => "146.80.48.247/24",tag => "zkmN",
         to =>
             #{addr => "65:f8:b9:5e:ce:71",dev => "nHkHdyBI",port => 100},
         vlan => 2525}],
  name => "Xm6M"}
```
From a template, you may create a link:
```
> Link = flop:template_link().
#{from =>
      #{addr => "36:36:8a:bc:f3:b3",dev => "zFndj2dc",port => 213},
  net => "186.49.222.41/24",tag => "Cd5U",
  to =>
      #{addr => "97:6c:1e:be:56:7e",dev => "A8YcaJHz",port => 23},
  vlan => 1925}

> flop:create(Link).
#{file => "foo",
  links =>
      [#{from =>
             #{addr => "6e:bb:ee:b9:4e:89",dev => "QtaXf3du",port => 142},
         id => "2152e93a832bc38bc08d0209ecf0e600d03d718a",
         net => "146.80.48.247/24",tag => "zkmN",
         to =>
             #{addr => "65:f8:b9:5e:ce:71",dev => "nHkHdyBI",port => 100},
         vlan => 2525},
       #{from =>
             #{addr => "36:36:8a:bc:f3:b3",dev => "zFndj2dc",port => 213},
         id => "f6e98bb3140a236e95aef2520217f4db429d09cd",
         net => "186.49.222.41/24",tag => "Cd5U",
         to =>
             #{addr => "97:6c:1e:be:56:7e",dev => "A8YcaJHz",port => 23},
         vlan => 1925}],
  name => "Xm6M"}
```
Update a link (using its ID)
```
> flop:update("215", tag, "bar").
#{file => "foo",
  links =>
      [#{from =>
             #{addr => "6e:bb:ee:b9:4e:89",dev => "QtaXf3du",port => 142},
         id => "2152e93a832bc38bc08d0209ecf0e600d03d718a",
         log =>
             [#{tag => "zkmN",until => "2023-02-17T07:57:44+01:00"}],
         net => "146.80.48.247/24",tag => "bar",
         to =>
             #{addr => "65:f8:b9:5e:ce:71",dev => "nHkHdyBI",port => 100},
         vlan => 2525},
       #{from =>
             #{addr => "36:36:8a:bc:f3:b3",dev => "zFndj2dc",port => 213},
         id => "f6e98bb3140a236e95aef2520217f4db429d09cd",
         net => "186.49.222.41/24",tag => "Cd5U",
         to =>
             #{addr => "97:6c:1e:be:56:7e",dev => "A8YcaJHz",port => 23},
         vlan => 1925}],
  name => "Xm6M"}
```
Delete a link
```
> flop:delete("f6").
#{file => "foo",
  links =>
      [#{from =>
             #{addr => "6e:bb:ee:b9:4e:89",dev => "QtaXf3du",port => 142},
         id => "2152e93a832bc38bc08d0209ecf0e600d03d718a",
         log =>
             [#{tag => "zkmN",until => "2023-02-17T07:57:44+01:00"}],
         net => "146.80.48.247/24",tag => "bar",
         to =>
             #{addr => "65:f8:b9:5e:ce:71",dev => "nHkHdyBI",port => 100},
         vlan => 2525}],
  name => "Xm6M"}
```
And save the database
```
> flop:save().
#{db =>
      #{'@' => "2023-02-17T08:11:06+01:00",file => "foo",
        id => "cb4f50c5c5b79d868a4ffaaff7911a0b471ac689",
        links =>
            [#{from =>
                   #{addr => "6e:bb:ee:b9:4e:89",dev => "QtaXf3du",port => 142},
               id => "2152e93a832bc38bc08d0209ecf0e600d03d718a",
               log =>
                   [#{tag => "zkmN",until => "2023-02-17T07:57:44+01:00"}],
               net => "124.89.195.226/24",tag => "N2Me",
               to =>
                   #{addr => "65:f8:b9:5e:ce:71",dev => "nHkHdyBI",port => 100},
               vlan => 255}],
        name => "5I0a"},
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
interface ethernet 1/142
     description cx=5I0a;to=nHkHdyBI_100
     switchport mode trunk
     switchport trunk allowed vlan 255
done
```
