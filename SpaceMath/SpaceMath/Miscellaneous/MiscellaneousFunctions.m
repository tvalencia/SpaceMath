(* Wolfram Language package *)

DeleteSpaceMath::usage="DeleteSpaceMath is an option of InstallSpaceMath. It specifies, the full path \
to the directory where SpaceMath will be installed.";

InstalledSpaceMath::usage="Directory SpaceMath"


Options[DeleteSpaceMath]=
{
	AutoDeleteSpaceMathDirectory-> None,
	InstalledSpaceMath->FileNameJoin[{$UserBaseDirectory, "Applications","SpaceMath"}]
};

DeleteSpaceMath[OptionsPattern[]]:=
Module[
 { 
  packageSM, packageDirSM, DeleteSM
 },
 packageSM = "SpaceMath";
 packageDirSM = OptionValue[InstalledSpaceMath];
 DeleteSM="Do you want to delete the SpaceMath package?";

 (* If the package directory already exists, ask the user about overwriting *)
 If[ DirectoryQ[packageDirSM],
  If[ OptionValue[AutoDeleteSpaceMathDirectory],
   Quiet@DeleteDirectory[packageDirSM, DeleteContents -> True],
   Null,
   If[ ChoiceDialog[DeleteSM,{"Yes, delete the " <> packageSM <>" package"->True,
    "No, I need it yet. Abort the deletion."->False}, WindowFloating->True, WindowTitle->"Existing SpaceMath Installation detected"],
	Quiet@DeleteDirectory[packageDirSM, DeleteContents -> True],
	Abort[]
	 ]
	]
   ];

 WriteString["stdout", "\nInstallation complete! Loading SpaceMath ... \n"];

];