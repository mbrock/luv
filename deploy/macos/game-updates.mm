// Sparkle's Cocoa UI, shared by the packaged SDL games. All calls use the
// main thread. The host completes its normal teardown before allowing relaunch.
#import <Cocoa/Cocoa.h>
#import <Sparkle/Sparkle.h>
#import <objc/runtime.h>

@interface GameUpdates : NSObject <SPUUpdaterDelegate, NSMenuItemValidation>
@property(nonatomic, strong) SPUStandardUpdaterController *controller;
@property(nonatomic, copy) void (^installHandler)(void);
@property(nonatomic) void (*quitGame)(void);
@end

@implementation GameUpdates
- (BOOL)updater:(SPUUpdater *)updater
    shouldPostponeRelaunchForUpdate:(SUAppcastItem *)item
    untilInvokingBlock:(void (^)(void))handler {
  self.installHandler = handler;
  self.quitGame();
  return YES;
}
- (void)check:(id)sender {
  [self.controller checkForUpdates:sender];
}
- (void)toggleChecks:(id)sender {
  self.controller.updater.automaticallyChecksForUpdates =
      !self.controller.updater.automaticallyChecksForUpdates;
}
- (BOOL)validateMenuItem:(NSMenuItem *)item {
  if (item.action == @selector(toggleChecks:)) {
    item.state = self.controller.updater.automaticallyChecksForUpdates
        ? NSControlStateValueOn : NSControlStateValueOff;
    return YES;
  }
  return self.controller.updater.canCheckForUpdates;
}
@end

static GameUpdates *updates;
extern "C" void game_updates_start(void (*quitGame)(void)) {
  NSCAssert([NSThread isMainThread], @"Updates require the main thread");
  if (updates || ![[NSBundle mainBundle] objectForInfoDictionaryKey:@"SUFeedURL"])
    return;
  updates = [GameUpdates new];
  updates.quitGame = quitGame;
  updates.controller = [[SPUStandardUpdaterController alloc]
      initWithStartingUpdater:YES updaterDelegate:updates userDriverDelegate:nil];
  NSMenu *menu = NSApp.mainMenu.itemArray.firstObject.submenu;
  if (menu) {
    NSMenuItem *check = [[NSMenuItem alloc] initWithTitle:@"Check for Updates…"
        action:@selector(check:) keyEquivalent:@""];
    check.target = updates;
    [menu insertItem:check atIndex:MIN(1, menu.numberOfItems)];
    NSMenuItem *automatic = [[NSMenuItem alloc]
        initWithTitle:@"Automatically Check for Updates"
        action:@selector(toggleChecks:) keyEquivalent:@""];
    automatic.target = updates;
    [menu insertItem:automatic atIndex:MIN(2, menu.numberOfItems)];
  }
}
extern "C" void game_updates_check(void) {
  [updates.controller checkForUpdates:nil];
}
extern "C" int game_updates_ready(void) {
  return updates && updates.controller.updater.canCheckForUpdates;
}
extern "C" void game_updates_finish(void) {
  NSCAssert([NSThread isMainThread], @"Updates require the main thread");
  if (!updates.installHandler)
    return;
  // SDL normally vetoes Cocoa termination to route it through the game loop.
  // The host has now saved and torn down; let Sparkle terminate and relaunch.
  NSApp.delegate = nil;
  // SDL's NSApplication subclass also overrides terminate: to enqueue a quit
  // event. Its loop is finished now, so restore Cocoa's real termination path.
  Method terminate = class_getInstanceMethod([NSApplication class], @selector(terminate:));
  class_replaceMethod(object_getClass(NSApp), @selector(terminate:),
      method_getImplementation(terminate), method_getTypeEncoding(terminate));
  void (^handler)(void) = updates.installHandler;
  updates.installHandler = nil;
  handler();
  [NSApp run];
}
