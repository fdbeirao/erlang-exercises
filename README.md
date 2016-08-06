# Fábio's Erlang scrapbook

A while ago I started looking into [Erlang](https://www.erlang.org/).

My mind was blown and still is, every day.

I am making an effort to learn Erlang. Not just the syntax, but the whole mindset that comes from designing software for the real (concurrent) world.

As a hardcore .NET C# developer, I am aware that this transition won't be the easiest. Yet, I firmly believe I will reap the benefits :)

This repo will work as my public scrapbook, so I can share my progress and keep track of some tricks.

**ProTip:**
Do you want to add an entry to your context menu that will open the Erlang shell in the current directory? 
![Erlang in the context menu](imgs/erlangContextMenu.png)

Easy :) open **regedit**, navigate to **HKEY_CLASSES_ROOT\Directory\Background\shell** and add the following keys:

```
Erlang\Icon (REG_SZ) = "C:\Program Files\erl8.0\bin\werl.exe"
Erlang\command\(Default) (REG_SZ) = "C:\Program Files\erl8.0\bin\werl.exe"
```

It should look something like this:

![Regedit](imgs/erlangContextMenu_regedit1.png)
![Regedit](imgs/erlangContextMenu_regedit2.png)

Fábio Beirão - 06/Aug/2016

---