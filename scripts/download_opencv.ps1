$opencv_ver=$args[0]
$opencv_cache_dir=$args[1]
$opencv_root_dir=$args[2]

$opencv_zip_url="https://github.com/opencv/opencv/archive/$opencv_ver.zip"
$opencv_source_zip="$opencv_cache_dir\\opencv-$opencv_ver.zip"
$opencv_source_root="$opencv_root_dir\\opencv-$opencv_ver"

if(-Not(Test-Path -PathType container -Path $opencv_cache_dir))
{
    New-Item -Path $opencv_cache_dir -ItemType Container
}
if(-Not(Test-Path -PathType container -Path $opencv_root_dir))
{
    New-Item -Path $opencv_root_dir -ItemType Container
}

if (-Not(Test-Path -Path $opencv_source_root -PathType container))
{
    if (-Not(Test-Path -Path $opencv_source_zip -PathType Leaf))
    {
        Invoke-WebRequest -Uri $opencv_zip_url -OutFile $opencv_source_zip
    }
    Expand-Archive -Path $opencv_source_zip -DestinationPath $opencv_root_dir
}
