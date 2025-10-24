# Quick Start

Now that Henceforth is installed, let's write and run a simple Hello World program.
This guide will help you go from zero to a running program in just a few steps.

## 1. Create a new file

First, create a new directory and a file for your program:

```bash
mkdir hello-world
cd hello-world
touch main.hfs
```

## 2. Write your first program

Open `main.hfs` in your favorite text editor and add the following code:

```
// main.hfs
fn main: () -> () {
  @(("Hello, world!")print (...)pop) return;
}
```

This simple program prints "Hello, world!" to the terminal. If you don't understand some of the syntax, you can learn about it in the next section, or in the language reference.

## 3. Run the program

Once your file is ready, run it from the command line:
```bash
henceforth main.hfs
```
Expected output:
```
Hello, world!
```

## 4. What's next?

You're all set up and running code! In the next section, you'll get a glimpse into some of the language features.
<hr>
<div style="
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-wrap: wrap;
  margin-top: 1em;
">
  <a href="installation.html"
     style="
       display: inline-block;
       padding: 8px 16px;
       border: 2px solid white;
       border-radius: 12px;
       color: white;
       text-decoration: none;
       font-weight: bold;
       background: transparent;
       transition: all 0.2s ease-in-out;
     "
     onmouseover="this.style.background='white'; this.style.color='#222';"
     onmouseout="this.style.background='transparent'; this.style.color='white';">
    ← Previous
  </a>

  <a href="first-program.html"
     style="
       display: inline-block;
       padding: 8px 16px;
       border: 2px solid white;
       border-radius: 12px;
       color: white;
       text-decoration: none;
       font-weight: bold;
       background: transparent;
       transition: all 0.2s ease-in-out;
     "
     onmouseover="this.style.background='white'; this.style.color='#222';"
     onmouseout="this.style.background='transparent'; this.style.color='white';">
    Next → Your First Henceforth Program
  </a>
</div>

