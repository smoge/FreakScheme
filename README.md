# FreakScheme: A Scheme Interpreter in Haskell


A Scheme interpreter written in Haskell, work in progress,

Welcome to FreakScheme, where the charm of Scheme meets the power of Haskell,
following the original insights of Adam Wespiser and his legendary "Write You A
Scheme, Version 2.0."


Eventually, I will use the experience of this project for more specific DSL interpreter projects.



## Inspiration

> F̴̡̨̧̨̢̧̧̧̨̛̥͇̪͖̼̻̗̣͉̬̳͙͖̗̳̤̖͕̙͕̩̘̗̬̜͈̝̩͈̜̗͓̬͕̦̰̞̦̱͈͔̱̟̺̘̟̣̙̩͎͔͚̩̬̮̝̼͔̗̪̳̤̞͕̲̤̥̞͇̪̩̗̱̤͖̠̲͕͈̳̫̖͙̲̙̫͈͉͙͕̜͚̝̰͉͕̳͈̬͈͉͈̟̝̯̱̬̪͌́̈́̅̓̓͐̏̉̈́͛͆͐͋͛̋̓̿͆́͊͐͑͐̀͊̆̄̃͋̉͛̽̅̌̽̂̋͒́͌̑̃̉̅̊̈́̐̌͗̕̕̕͜͠͠͝͝ͅͅè̴̢̢̢̧̢̢̧̧̢̢̡͖̙͍̮̙̯̯̖̠̭̝͕̠̞̼͉̠̠̭̗͈̯̭̱̮̖̫̤̲̣̯͙̭͔̜͈̤̻͓̬̞̪͉̺̣̜̭͈̗̞̟̟̫͉͈̼̘̺̲͈̥͚̜̪̟̠͙̳͍̣̙̰͍̯̭͕̠̘͔͎̦̦͕̪̫̏̈̄̌͑̑̉̄̃͑̾̚̕̕͜͜͝͠ͅͅͅę̵̛͚͎̼̲͇̣͇̆̄̓̒̃̒̆̏̑̀̓́̍͂́̄̈̒͑͆̓̓̈́̈́́͒̂̾́̐̏͆̽̔̈́́̕̚͘͝͝͝͝
> F̶̛̛͙͍͇͍̼̽̐̇́̈́͊͛̑̈̅́͆̐́̀̇̽̓̄̿̀̋̎̌̒̊̄̄͐̉͆́͛͂͛̑͆͂̃͒̋͌̈́͌͆̈́̓̇͗͐́̑̽̀͊͗̍͘̚̚̚̚͝͝e̸̢̢̡̧̡̨̢̛͓͎̥͖̪͔̻̘̙̗̻̻͕̹̠̖̠͓̳̞̬̯̦͇͔̹͖̱̖̲͉͍͕̙̺̫̻̙̭͕̭̠͇̺̿̊̉̍̂̔͌͛̅̊̉́̈́͋̒̀̀͆̉̐͊͌̑͂̉̂̃͛̆͗͊̌̑̄́͌̔̈́̃̔̓̑͗̐̑͒̾͒͆͌̍̇̿̚̚̕͘̚̕ȩ̷̛̛̜̺̻̞̲̾̇͒̾̇̓̆́̈́̅̒̉̓̈́̿̐̉̍͒̏̏͛̈́̿́̍̔̅̀͋͂̈́͛͊̅͐̈́͛̀͂̍͒̌͊̀̌̄̍̇̆̀̚͝l̶̛̞͎̩̪͚̬̙̪̰͇͖̜͉͔̖͑͊̊̐̄̒͆̉̍̀͐̒̃̇̊̋̎̓̓͂̄͂̎͗̇͑͆̐̈́̃̉̆́̐͂̑͋͑́̀̅̑̈̒̂̓̇́̌̈́́̉͝͝l̵̡̛̮̮̪͕͎̺͎̝̪̝͚̖̮̜̰͉̣͖͙̬̪̩̻̼̈́̇̐̿̅̉̓̄̐̇̔̑̅̈́̇̂̾̔̾̀̀̏̽̈́̊̈́́̐͌̊̑̊̃́̂̓̄̐̊̐̉̈̄̍̏̍͗͂͆̌̑͋̄̾͆̍̓̏̈̚̕̕̚͘̚̚͝͝ę̷̡͓̟̱͓̞̬̫̭̲̼͈̣̦̫̤̼̬̹͉̻̘̙̱̗̻͍̰̂̏̂̅̆̋͐̈́̽̌̈́̽͆̍͒̈́̔͋̾̈́͂́̔̈́̈́̿̋̐̐͌͗̂̎̀͆͛̈́̇̓̉̍̈͑̔̃̈̐̀̑̊͐̒͘͘͘̚̚̚͜͝ḁ̵̢̢̨̡̡̡̢̭̲̻͖̮͇͓͉͔̹̟̤̱̗̣͖̙̘̼̭̹͔̳̠͚̝̳͖̪̪͕͍͖̘͎̤̪̭͙̔̽͂̓̑̋̾̾͐̉̑͐̓͒̈́̍̋͑̉̐̄̀͆̓̍͑͛͛͐͋́̍̄̀͊̔̂̇̑̀̆͑́͐́̚̕͘̕͠͠͝e̵̛̛̛͈̭͈̺̬̜̱̬͋̌̍̿͂̋͊́̈̔̋͆̅̐̆̈́̀̾̋̄͒̀̉͒̓́̓͗͋̀͐̄͂̋̿̏̿̾̑̈́́͆̈͘͘̕̚͘̚͠͠͝͝
>
>

The project draws inspiration from the insightful work of Adam Wespiser, who penned "Write You A Scheme, Version 2.0". His work serves as a guiding light for this project.

## 📜 The Sacred Scrolls

FreakScheme is bound by the GLP License, a testament to FREEDOM.

![alt text](whatever/s3.jpg) 
