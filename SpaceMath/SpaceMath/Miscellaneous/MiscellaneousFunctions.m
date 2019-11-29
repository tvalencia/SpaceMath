(* Wolfram Language package *)

DeleteSpaceMath::usage="DeleteSpaceMath is an option of InstallSpaceMath. It specifies, the full path \
to the directory where SpaceMath will be installed.";

InstalledSpaceMath::usage="Directory SpaceMath"


Options[DeleteSpaceMath]=
{
	AutoOverwriteSpaceMathDirectory-> None,
	InstalledSpaceMath->FileNameJoin[{$UserBaseDirectory, "Applications","SpaceMath"}]
};

DeleteSpaceMath[OptionsPattern[]]:=
Module[
 { 
  packageName, packageDir, OverwriteSM
 },
 packageName = "SpaceMath";
 packageDir = OptionValue[InstalledSpaceMath];
 OverwriteSM="Do you want to delete the SpaceMath package?";

 (* If the package directory already exists, ask the user about overwriting *)
 If[ DirectoryQ[packageDir],
  If[ OptionValue[AutoOverwriteSpaceMathDirectory],
   Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
   Null,
   If[ ChoiceDialog[OverwriteSM,{"Yes, delete the " <> packageName <>" package"->True,
    "No, I need to do a backup first. Abort installation."->False}, WindowFloating->True, WindowTitle->"Existing SpaceMath Installation detected"],
	Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
	Abort[]
	 ]
	]
   ];

 WriteString["stdout", "\nInstallation complete! Loading SpaceMath ... \n"];

];