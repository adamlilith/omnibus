#' Find screen resolution
#'
#' This function finds the screen resolution of the current device. It should work on Windows, macOS, and Linux systems.
#'
#' @examples
#' screenRes()
#'
#' @returns A list with two elements: `width` and `height`, which are the width and height of the screen in pixels.
#'
#' @aliases screenRes
#' @rdname screenRes
#' @export screenRes
screenRes <- function() {

  	os <- Sys.info()['sysname']

	if (os == 'Windows') {

		# res <- system('powershell -command "Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.Screen]::PrimaryScreen.Bounds.Size.Width, [System.Windows.Forms.Screen]::PrimaryScreen.Bounds.Size.Height"', intern = TRUE)

		# res <- system('powershell -command "Add-Type -TypeDefinition \'
		# 	using System;
		# 	using System.Runtime.InteropServices;
		# 	public class ScreenRes {
		# 		[DllImport(\\"user32.dll\\")]
		# 		public static extern int GetSystemMetrics(int nIndex);
		# 	}\';
		# 	[ScreenRes]::GetSystemMetrics(0), [ScreenRes]::GetSystemMetrics(1)"', intern = TRUE)

    	# res <- as.numeric(res)
		# out <- c(width = res[2L], height = res[1L])

		winVersion <- system('powershell -command "(Get-WmiObject Win32_OperatingSystem).Caption"', intern = TRUE)

		if (grepl('Windows 7', winVersion) || grepl('Windows 8', winVersion)) {

			# Windows 7 and 8: Use wmic
			res <- system('wmic desktopmonitor get screenheight, screenwidth', intern = TRUE)
			res <- as.numeric(unlist(strsplit(res[2], "\\s+")))

		} else {
		# Windows 10 and 11: Use PowerShell

			res <- system('powershell -command "Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.Screen]::PrimaryScreen.Bounds.Size.Width, [System.Windows.Forms.Screen]::PrimaryScreen.Bounds.Size.Height"', intern = TRUE)

			res <- as.numeric(unlist(res))

		}

		out <- c(width = res[2L], height = res[1L])

	} else if (os == 'Darwin') {  # macOS

		res <- system("system_profiler SPDisplaysDataType | grep Resolution", intern = TRUE)
		res <- gsub("[^0-9x]", "", res)  # Clean output
		res <- unlist(strsplit(res, "x"))
		out <- c(width = as.numeric(res[1L]), height = as.numeric(res[2L]))

	} else if (os == 'Linux') {

		res <- system("xrandr | grep '*' | awk '{print $1}'", intern = TRUE)
		res <- unlist(strsplit(res, "x"))
		out <- c(width = as.numeric(res[1L]), height = as.numeric(res[2L]))

	} else {
		stop("Unsupported operating system.")
	}
	out

}
