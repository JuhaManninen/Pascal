{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit uListeners;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
  Changed now, interfaces are not used any more:
  (Listener interface for the objectmodel)

  (Object that are listeners have to inherit from TComponent.
   If TInterfaceObject is used there will be an error in refcount.
   TComponent implements it's own queryinterface and addref.)

 alternativ implementation:
   TMethodReference
   AddListener(Self, [ [mtBeforeChange,OnBeforeChange] , [mtBeforeAdd,OnBeforeAdd] ]);
}

interface

type

  // Original interfaces moved to text file "Interfaces.txt" and the same names
  // are used for enum.
  // Methods defined in these interfaces must be defined in implementing classes.
{  IObjectModelListener = interface(IUnknown)
    procedure Change(Sender: TModelEntity);
  end;
  IModelEntityListener = interface(IObjectModelListener)
    procedure AddChild(Sender: TModelEntity; NewChild: TModelEntity);
    procedure Remove(Sender: TModelEntity);
    procedure EntityChange(Sender: TModelEntity);
  end;  }

  TListenerType = (
    ltObjectModelListener,        // IObjectModelListener,
    ltBeforeObjectModelListener,  // IBeforeObjectModelListener,
    ltAfterObjectModelListener,   // IAfterObjectModelListener,
    ltModelEntityListener,        // IModelEntityListener,
    ltAbstractPackageListener,    // IAbstractPackageListener,
    ltUnitPackageListener,        // IUnitPackageListener,
    ltBeforeUnitPackageListener,  // IBeforeUnitPackageListener,
    ltAfterUnitPackageListener,   // IAfterUnitPackageListener,
    ltLogicPackageListener,       // ILogicPackageListener,
    ltBeforeLogicPackageListener, // IBeforeLogicPackageListener,
    ltAfterLogicPackageListener,  // IAfterLogicPackageListener,
    ltClassifierListener,         // IClassifierListener,
    ltClassListener,              // IClassListener,
    ltBeforeClassListener,        // IBeforeClassListener,
    ltAfterClassListener,         // IAfterClassListener,
    ltInterfaceListener,          // IInterfaceListener,
    ltBeforeInterfaceListener,    // IBeforeInterfaceListener,
    ltAfterInterfaceListener,     // IAfterInterfaceListener,
    ltDatatypeListener,           // IDatatypeListener,
    ltBeforeDatatypeListener,     // IBeforeDatatypeListener,
    ltAfterDatatypeListener,      // IAfterDatatypeListener,
    ltFeatureListener,            // IFeatureListener,
    ltBeforeFeatureListener,      // IBeforeFeatureListener,
    ltAfterFeatureListener,       // IAfterFeatureListener,
    ltOperationListener,          // IOperationListener,
    ltBeforeOperationListener,    // IBeforeOperationListener,
    ltAfterOperationListener,     // IAfterOperationListener,
    ltAttributeListener,          // IAttributeListener,
    ltBeforeAttributeListener,    // IBeforeAttributeListener,
    ltAfterAttributeListener,     // IAfterAttributeListener,
    ltPropertyListener,           // IPropertyListener,
    ltBeforePropertyListener,     // IBeforePropertyListener,
    ltAfterPropertyListener,      // IAfterPropertyListener,
    ltParameterListener,          // IParameterListener,
    ltBeforeParameterListener,    // IBeforeParameterListener,
    ltAfterParameterListener);    // IAfterParameterListener);

  TListenerTypes = set of TListenerType;

////////////////////////////////
////////////////////////////////
implementation

end.