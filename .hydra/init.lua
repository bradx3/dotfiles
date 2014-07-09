-- Hi!
-- Save this as ~/.hydra/init.lua and choose Reload Config from the menu

hydra.alert("Config loaded", 1.5)

-- open a repl
--   the repl is a Lua prompt; type "print('hello world')"
--   when you're in the repl, type "help" to get started
--   almost all readline functionality works in the repl
hotkey.bind({"cmd", "ctrl", "alt"}, "R", repl.open)

-- save the time when updates are checked
function checkforupdates()
  updates.check()
  settings.set('lastcheckedupdates', os.time())
end

-- show a helpful menu
menu.show(function()
    local updatetitles = {[true] = "Install Update", [false] = "Check for Update..."}
    local updatefns = {[true] = updates.install, [false] = checkforupdates}
    local hasupdate = (updates.newversion ~= nil)

    return {
      {title = "Reload Config", fn = hydra.reload},
      {title = "-"},
      {title = "Work setup", fn = worksetup},
      {title = "-"},
      {title = "About", fn = hydra.showabout},
      {title = updatetitles[hasupdate], fn = updatefns[hasupdate]},
      {title = "Quit Hydra", fn = os.exit},
    }
end)

-- move the window to the right a bit, and make it a little shorter
hotkey.new({"cmd", "ctrl", "alt"}, "J", function()
    local win = window.focusedwindow()
    local frame = win:frame()
    frame.x = frame.x + 10
    frame.h = frame.h - 10
    win:setframe(frame)
end):enable()

-- show available updates
local function showupdate()
  os.execute('open https://github.com/sdegutis/Hydra/releases')
end

-- what to do when an udpate is checked
function updates.available(available)
  if available then
    notify.show("Hydra update available", "", "Click here to see the changelog and maybe even install it", "showupdate")
  else
    hydra.alert("No update available.")
  end
end

-- Uncomment this if you want Hydra to make sure it launches at login
autolaunch.set(true)

-- check for updates every week
timer.new(timer.weeks(1), checkforupdates):start()
notify.register("showupdate", showupdate)

-- if this is your first time running Hydra, you're launching it more than a week later, check now
local lastcheckedupdates = settings.get('lastcheckedupdates')
if lastcheckedupdates == nil or lastcheckedupdates <= os.time() - timer.days(7) then
  checkforupdates()
end




-- I've worked hard to make Hydra useful and easy to use. I've also
-- released it with a liberal open source license, so that you can do
-- with it as you please. So, instead of charging for licenses, I'm
-- asking for donations. If you find it helpful, I encourage you to
-- donate what you believe would have been a fair price for a license:

local function donate()
  os.execute("open 'https://www.paypal.com/cgi-bin/webscr?business=sbdegutis@gmail.com&cmd=_donations&item_name=Hydra.app%20donation'")
end

hotkey.bind({"cmd", "alt", "ctrl"}, "D", donate)

--- End of default config

--- Custom config

-- To find out bundle id:
-- osascript -e 'id of app "[app name]"'

-- Hydra docs
-- http://sdegutis.github.io/hydra/application.html

local function myreload()
   hydra.reload()
   worksetup()
end
hotkey.bind({"cmd", "ctrl"}, "W", myreload)

local function mydebug()
   hydra.reload()
--   printframeinfo("com.googlecode.iterm2", 1)
--   printframeinfo("net.phere.GitX", 1)
--   moveappwindow2("com.googlecode.iterm2", 1112, 123, 802, 809, "1.")
--   moveappwindow2("com.googlecode.iterm2", 1112, 123, 802, 809, "2.")

--   moveappwindow("net.phere.GitX", 253, 1080, 1440, 739, 1)
   printframeinfo("net.phere.GitX")
end
hotkey.bind({"cmd", "ctrl"}, "D", mydebug)

--- Set up apps how I like it at work
function worksetup()
   hydra.alert("Setting up for work")
   moveappwindow("org.gnu.Emacs", 333, 22, 1259, 1050, 1)
   moveappwindow("com.hipchat.HipChat", 241, 22, 1364, 826, 1)
   moveappwindow("com.apple.iChat", 411, 1080, 1149, 698, 1)
   moveappwindow("org.mozilla.firefox", 316, 22, 1217, 826, 1)
   moveappwindow("com.apple.mail", 262, 22, 1356, 826, 1)
   moveappwindow("net.phere.GitX", 253, 1080, 1440, 739, 1)
   moveappwindow("com.13bold.Bowtie", 249, 1882, 314, 104, 1)
   moveappwindow2("com.googlecode.iterm2", 1112, 123, 802, 809, "1.")
   moveappwindow2("com.googlecode.iterm2", 476, 1083, 1018, 809, "2.")
   moveappwindow2("com.googlecode.Chrome", 1045, 0, 875, 863, "Developer Tools")
   moveappwindow2("com.googlecode.Chrome", 0, 0, 1024, 863, "")

   -- hydra.alert("Arranging Chrome")
   -- local chrome = application.applicationsforbundleid("com.google.Chrome")[1]
   -- local window = application.visiblewindows(chrome)[2]
   -- for i, w in ipairs(application.visiblewindows(chrome)) do
   --    if string.find(w:title(), "Developer Tools") then
   -- 	 movewindow(w, 1045, 0, 875, 863)
   --    else
   -- 	 movewindow(w, 0, 0, 1024, 863)
   --    end
   -- end
   -- chrome:activate()
end

function activate(app)
   for i=1,10 do
      if app:activate() and #application.visiblewindows(app) > 1 then
	 hydra.alert("Found on attempt " .. i)
	 break
      else
	 os.execute("sleep 0.25")
      end
   end
end

function findwindow(app, substring)
   if substring == nil then
      substring = ""
   end

   local windows = application.allwindows(app)
   for i, w in ipairs(windows) do
      hydra.alert(w:title())
      if string.find(w:title(), substring) then
	 return w
      end
   end
end

function moveappwindow2(bundle, x, y, width, height, titlesubstring)
   local app = application.applicationsforbundleid(bundle)[1]
   activate(app)

   local window = findwindow(app, titlesubstring)
   movewindow(window, x, y, width, height)
end

function printframeinfo(bundle, titlesubstring)
   local app = application.applicationsforbundleid(bundle)[1]
   activate(app)

   local window = findwindow(app, titlesubstring)
   local frame = window:frame()
   local sub = "\"\""
   if titlesubstring then
      sub = "\"" .. titlesubstring .. "\""
   end
   local str = "moveappwindow(\"" .. bundle .. "\", " .. frame.x .. ", " .. frame.y .. ", " .. frame.w .. ", " .. frame.h .. ", " .. sub .. ")"
   os.execute("echo '" .. str .. "' >> /tmp/hydra")
end

function moveappwindow(bundle, x, y, width, height, windowIndex)
   local app = application.applicationsforbundleid(bundle)[windowIndex]
   hydra.alert("Arranging " .. bundle)
   app:activate()

   -- sleep to wait for any change of screens animation to complete
   os.execute("sleep 2")

   local window = application.allwindows(app)[1]
   movewindow(window, x, y, width, height)
end

function movewindow(window, x, y, width, height)
   local frame = window:frame()
   frame.x = x
   frame.y = y
   if width then
      frame.w = width
   end
   if height then
      frame.h = height
   end
   window:setframe(frame)
end
