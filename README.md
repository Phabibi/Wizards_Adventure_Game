# An adventure game using common LISP
In this game, you are a wizard’s apprentice. You’ll explore the wizard’s house. As you can see, we can visit three different locations: a living room, an attic, and a garden. Players can move between places using the door and the ladder to the attic.



![alt text](http://www.lisperati.com/world.jpg)

## Requirments
  - download Clisp https://clisp.sourceforge.io/
## How to play! 

 - type ``` clisp ``` in the command line to launch clisp interpreter 
 - ``` (load 'Wizards_Adventures) ```
 - to start the game : `(startgame)`
 - to exit the game : `(bye)`
 ## Supported movements 
   - ```  look  ``` : gives you information of your surrounding 
   - ``` walk {direction} ``` walks you to a direction and then performs ``` look ``` to describe the surrounding
   - ``` pickup {item_name} ``` picks up an item and puts it into inventory 
   - ``` inventory ``` shows your current inventory 
   - ``` drop {item_name} ``` drops the selecet item and performs ``` inventory ``` 
#### more to come ... 
  
 
