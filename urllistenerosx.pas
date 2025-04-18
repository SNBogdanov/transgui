

{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}

Unit URLListenerOSX;
{$mode objfpc}{$H+}
{$modeswitch objectivec2}

Interface

Uses 
Classes, SysUtils, CocoaAll, InternetConfig, AppleEvents;

Type 
  THandlerProc = Procedure (Const url: String);

  { TAppURLHandler }

  TAppURLHandler = objcclass(NSObject)
                   Public 
                     Procedure  getUrlwithReplyEvent(event:
                                                     NSAppleEventDescriptor;
                                                     eventReply:
                                                     NSAppleEventDescriptor);
                     message 'getUrl:withReplyEvent:';
                   Public 
                     callBack: THandlerProc;
End;

Procedure RegisterURLHandler(HandlerProc: THandlerProc);

Var 
  handler : TAppURLHandler;
  eventManager: NSAppleEventManager;

Implementation

{ TAppURLHandler }

Procedure TAppURLHandler.getUrlwithReplyEvent(event: NSAppleEventDescriptor;
                                              eventReply: NSAppleEventDescriptor
);

Var 
  url : NSString;
Begin
  url := event.paramDescriptorForKeyword(keyDirectObject).stringValue;
  callBack(url.UTF8String);
End;

Procedure RegisterURLHandler(HandlerProc: THandlerProc);
Begin
  handler := TAppURLHandler.alloc.init;
  handler.callBack := HandlerProc;
  eventManager := NSAppleEventManager.sharedAppleEventManager;
  eventManager.setEventHandler_andSelector_forEventClass_andEventID(handler,
                                                                    ObjCSelector
                                                                    (handler.

                                                            getUrlwithReplyEvent
  ), kInternetEventClass,kAEGetURL);
End;

End.
