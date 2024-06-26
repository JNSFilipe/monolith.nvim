#ifndef UTILS_H_
#define _XOPEN_SOURCE 700
#define UTILS_H_

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#include <unistd.h>
#endif

#define MONO_WGT_BOLD "\e[1m"
#define MONO_WGT_RGLR "\e[m"

#define MONO_COLR_YLW "\033[0;33m"
#define MONO_COLR_RED "\033[0;31m"
#define MONO_COLR_GRN "\033[0;32m"
#define MONO_COLR_BLU "\033[0;34m"
#define MONO_COLR_RST "\033[0m"

#define MONO_ASSERT assert
#define MONO_REALLOC realloc
#define MONO_FREE free

// TODO: rename to MONO_MAX_STR_SZ
#define MAX_OUT_SZ 1024

typedef enum {
  MONO_INFO,
  MONO_WARNING,
  MONO_ERROR,
} Mono_Log_Level;

// MACROS

/**
 * MONO_PRINT_SEP Macro
 *
 * Print an horizonal separator to stderr
 *
 * Returns:
 *   void
 *
 */
#define MONO_PRINT_SEP                                                         \
  fprintf(stderr, "------------------------------------\n");
// MACROS

/**
 * MONO_ARRAY_NUMEL Macro
 *
 * Returns the number of elements of a static C vector
 *
 * Returns:
 *   The number of elements of the array (int)
 *
 */
#define MONO_ARRAY_NUMEL(arr) (int)(sizeof(arr) / sizeof(arr[0]))

/**
 * MONO_CMD Macro
 *
 * Runs commands in the interactive shell and returns result
 *
 * Returns:
 *   A dynamically allocated string containing the resolved string. Empty on
 * failure.
 *
 * Note:
 *   - The caller is responsible for freeing the returned string to avoid memory
 * * leaks.
 *   - Ensure to check for empty string return in case of failure.
 */
char *run_cmd(char *input) {
  char cmd[MAX_OUT_SZ];
  char *output = (char *)malloc(MAX_OUT_SZ * sizeof(char));
  snprintf(cmd, sizeof(cmd), input, input);
  FILE *fp = popen(cmd, "r");
  if (fp == NULL) {
    output[0] = '\0';
    return output;
  }

  if (fgets(output, MAX_OUT_SZ, fp) == NULL)
    output[0] = '\0';

  pclose(fp);
  return output;
}
#define MONO_CMD(input) run_cmd(input)

/**
 * MONO_SYS Macro
 *
 * Runs commands in the interactive shell, using system function
 *
 * Returns:
 *   system() error code
 *
 */
#define MONO_SYS(input) system(input)

/**
 * MONO_RESOLVE_ENV_VARS Macro
 *
 * Resolves env vars, by running 'echo <comand>' and getting the result
 *
 * Returns:
 *   A dynamically allocated string containing the resolved string. Empty on
 * failure.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses 'echo' function.
 *   - Windows: Uses 'echo' function from WinAPI.
 *   TODO: maybe need to replace $VAR by %VAR% on Windows
 *
 * Note:
 *   - The caller is responsible for freeing the returned string to avoid memory
 * * leaks.
 *   - Ensure to check for empty string return in case of failure.
 */
#ifdef _WIN32
#define ECHO_CMD "echo "
#else
#define ECHO_CMD "echo -n "
#endif
char *echo_eval(char *input) {
  char cmd[MAX_OUT_SZ];
  snprintf(cmd, sizeof(cmd), ECHO_CMD "%s", input);

  return MONO_CMD(cmd);
}
#define MONO_RESOLVE_ENV_VARS(input) echo_eval(input)

/**
 * MONO_PWD Macro
 *
 * Retrieves the current working directory as a string.
 *
 * Returns:
 *   A dynamically allocated string containing the current directory path on
 * success. NULL on failure.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses 'getcwd' function.
 *   - Windows: Uses 'GetCurrentDirectoryA' function from WinAPI.
 *
 * Note:
 *   - The caller is responsible for freeing the returned string to avoid memory
 * * leaks.
 *   - Ensure to check for NULL return in case of failure.
 */
#ifdef _WIN32
char *get_current_dir_windows() {
  DWORD needed = GetCurrentDirectoryA(0, NULL);
  char *buffer = (char *)malloc(needed);
  if (buffer == NULL) {
    return NULL; // Allocation failed
  }

  if (GetCurrentDirectoryA(needed, buffer) == 0) {
    free(buffer);
    return NULL; // Failed to get directory
  }

  return buffer;
}
#define MONO_PWD() get_current_dir_windows()

#else
char *get_current_dir_unix() {
  char *buffer = (char *)malloc(MAX_OUT_SZ);
  if (buffer == NULL) {
    return NULL; // Allocation failed
  }

  if (getcwd(buffer, MAX_OUT_SZ) == NULL) {
    free(buffer);
    return NULL; // Failed to get directory
  }

  return buffer;
}
#define MONO_PWD() get_current_dir_unix()

#endif

/**
 * MONO_SYMLINK Macro
 *
 * Creates a symbolic link from 'dest' to 'src'.
 *
 * Parameters:
 *   - src (const char*): Path to the target of the symlink.
 *   - dest (const char*): Path where the symlink will be created.
 *
 * Returns:
 *   0 on success, -1 on failure.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses 'symlink' function.
 *   - Windows: Uses 'CreateSymbolicLinkA' function from WinAPI.
 *
 * Note: Requires appropriate permissions on the respective platform.
 */
#ifdef _WIN32
#define MONO_SYMLINK(src, dest) (CreateSymbolicLinkA(dest, src, 0) ? 0 : -1)
#else
#define MONO_SYMLINK(src, dest) symlink(src, dest)
#endif

/**
 * MONO_EXISTS Macro
 *
 * Checks if a file or directory exists at the given path.
 *
 * Parameters:
 *   - path (const char*): The path to the file or directory.
 *
 * Returns:
 *   1 (true) if the file or directory exists, 0 (false) otherwise.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses 'stat' function.
 *   - Windows: Uses 'GetFileAttributesA' function from WinAPI.
 *
 *
 *
 */
#ifdef _WIN32
#define MONO_EXISTS(path) (GetFileAttributesA(path) != INVALID_FILE_ATTRIBUTES)
#else
#define MONO_EXISTS(path) (stat(path, &(struct stat){0}) == 0)
#endif

/**
 * MONO_MK_FULL_PATH Macro
 *
 * Creates the full directory path specified by 'path', including all necessary
 * subdirectories.
 *
 * Parameters:
 *   - path (const char*): The full path to be created.
 *
 * Returns:
 *   0 on success, -1 on failure.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses 'mkdir' function from sys/stat.h.
 *   - Windows: Uses 'CreateDirectoryA' function from windows.h.
 *
 * Note: The macro relies on the create_full_path function to handle the
 * creation of subdirectories in the path.
 * All directories must be followed by a path separator ('\' in Windows, '/' in
 * Linux). If they do not terminate with the separator, is assumed it is a file
 * and folder won't be created.
 */
#ifdef _WIN32
#define MONO_MKDIR(path) CreateDirectoryA(path, NULL)
#define PATH_SEPARATOR '\\'
#else
#define MONO_MKDIR(path) mkdir(path, 0777)
#define PATH_SEPARATOR '/'
#endif

int create_full_path(const char *path) {
  // TODO: there is probably a way to do this more efficiently
  char tmp[MAX_OUT_SZ];
  size_t len;
  len = strlen(path);
  for (int i = 0; i < len; i++) {
    if (path[i] == PATH_SEPARATOR) {
      memcpy(tmp, path, (MAX_OUT_SZ - 1) * sizeof(char));
      tmp[i + 1] = '\0'; // place null char
      if (!MONO_EXISTS(tmp)) {
        if (MONO_MKDIR(tmp) != 0)
          return -1;
      }
    }
  }
  return 0;
}

#define MONO_MK_FULL_PATH(path) create_full_path(path)

/**
 * MONO_MV Macro
 *
 * Moves a file or directory from 'src' to 'dest'.
 *
 * Parameters:
 *   - src (const char*): The path of the source file or directory.
 *   - dest (const char*): The path to move the file or directory to.
 *
 * Returns:
 *   0 on success, -1 on failure.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses the 'rename' function, which can move files and
 * directories within the same filesystem.
 *   - Windows: Uses the 'MoveFileA' function from the WinAPI, which moves files
 * or directories.
 *
 * Note: Moving directories across different filesystems may not be supported on
 * all platforms.
 */
#ifdef _WIN32
#define MONO_MV(src, dest) (MoveFileA(src, dest) ? 0 : -1)
#else
int mv_file_or_dir(const char *input, const char *output) {
  char cmd[MAX_OUT_SZ];
  snprintf(cmd, sizeof(cmd), "mv -f %s %s", input, output);

  MONO_CMD(cmd);

  return 0;
}
#define MONO_MV(src, dest) mv_file_or_dir(src, dest)
#endif

/**
 * MONO_DEL_MV_TO_TMP Macro
 *
 * 'Soft' deletes a file or directory by moving it to a temporary directory.
 * On Unix, it uses '/tmp'. On Windows, it uses the directory specified by the
 * 'TEMP' environment variable.
 *
 * Parameters:
 *   - path (const char*): The path of the file or directory to delete.
 *
 * Returns:
 *   0 on success, -1 on failure.
 *
 * Notes:
 *   - The macro generates a new path by appending the filename or directory
 * name to the temp directory path.
 *   - If the file or directory name is not unique in the temp directory, the
 * move may fail or overwrite existing files.
 *   - This operation is a 'soft' delete and can be reversed by manually moving
 * the file or directory back from the temp directory.
 */
#ifdef _WIN32
#define TMP_DIR getenv("TEMP")
#else
#define TMP_DIR "/tmp/"
#endif
int mv_to_tmp(const char *input) {
  MONO_MV(input, TMP_DIR);
  return 0;
}
#define MONO_DEL_MV_TO_TMP(path) mv_to_tmp(path)

/**
 * MONO_DEL Macro
 *
 * Deletes a file or directory specified by 'path'.
 *
 * Parameters:
 *   - path (const char*): The path of the file or directory to be deleted.
 *
 * Returns:
 *   0 on success, -1 on failure.
 *
 * Behavior varies by platform:
 *   - Unix (Linux, macOS): Uses 'remove' function.
 *   - Windows: Uses 'DeleteFileA' for files and 'RemoveDirectoryA' for
 * directories.
 *
 * Note:
 *   - On Windows, the function distinguishes between files and directories.
 *   - On Unix, the 'remove' function handles both files and empty directories.
 */
#ifdef _WIN32
int delete_file_or_dir(const char *path) {
  DWORD attrs = GetFileAttributesA(path);
  if (attrs == INVALID_FILE_ATTRIBUTES) {
    return -1; // Path not found or error
  }

  if (attrs & FILE_ATTRIBUTE_DIRECTORY) {
    return RemoveDirectoryA(path) ? 0 : -1;
  } else {
    return DeleteFileA(path) ? 0 : -1;
  }
}
#else
int delete_file_or_dir(const char *path) {
  char cmd[MAX_OUT_SZ];
  snprintf(cmd, sizeof(cmd), "rm -rf %s", path);

  MONO_CMD(cmd);

  return 0;
}
#endif
// TODO: Check Tsoding for better implementation, problably
#define MONO_DEL(path) (delete_file_or_dir(path) == 0 ? 0 : -1)

// Functions

/**
 * mono_log Function
 *
 * Logs a message with a specified severity level. The message format and
 * arguments are similar to printf. The log message is printed to the standard
 * error stream (stderr).
 *
 * Parameters:
 *   - level (Log_Level): The severity level of the log message. Should be one
 * of the predefined levels: INFO, WARNING, or ERROR.
 *   - fmt (const char*): A format string as in printf that specifies how
 * subsequent arguments (accessed via the variable-length argument facilities of
 * stdarg) are converted for output.
 *   - ... : A variable number of additional arguments. These are the values to
 * be printed. Their number and types must correspond to the format string.
 *
 * Usage:
 *   log(INFO, "This is an %s message with number %d", "info", 1);
 *
 * Notes:
 *   - The function will prefix the message with a tag based on the log level
 *     (e.g., "[INFO] " for INFO level).
 *   - Messages are followed by a newline character.
 *   - In the default case, where the log level is not recognized, an assertion
 * is triggered indicating an unreachable code path.
 *   - This function uses variable arguments and requires <stdarg.h> for proper
 * operation.
 *   - Designed for logging messages to stderr, useful for debugging and error
 * reporting.
 */
void mono_log(Mono_Log_Level level, const char *fmt, ...) {
  switch (level) {
  case MONO_INFO:
    fprintf(stderr, MONO_COLR_GRN);
    fprintf(stderr, MONO_WGT_BOLD);
    fprintf(stderr, "[INFO] ");
    fprintf(stderr, MONO_COLR_RST);
    fprintf(stderr, MONO_WGT_BOLD);
    break;
  case MONO_WARNING:
    fprintf(stderr, MONO_COLR_YLW);
    fprintf(stderr, MONO_WGT_BOLD);
    fprintf(stderr, "[WARNING] ");
    fprintf(stderr, MONO_COLR_RST);
    fprintf(stderr, MONO_WGT_BOLD);
    break;
  case MONO_ERROR:
    fprintf(stderr, MONO_COLR_RED);
    fprintf(stderr, MONO_WGT_BOLD);
    fprintf(stderr, "[ERROR] ");
    fprintf(stderr, MONO_COLR_RST);
    fprintf(stderr, MONO_WGT_BOLD);
    break;
  default:
    fprintf(stderr, MONO_WGT_RGLR);
    MONO_ASSERT(0 && "unreachable");
  }

  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, MONO_WGT_RGLR);
  fprintf(stderr, "\n");
}

/**
 * mono_download_file Function
 *
 * Downloads a file from a specified URL using libcurl and saves it locally.
 * Uses callback function to write downloaded data to a file.
 *
 * Parameters:
 *   - url (const char*): The URL from which to download the file.
 *   - output_filename (const char*): The filename to save the downloaded file
 * locally.
 *
 * Returns:
 *   - int: Returns 0 on success, non-zero on failure.
 *
 * Usage:
 *   int result =
 * download_file("https://download.jetbrains.com/fonts/JetBrainsMono-2.304.zip",
 * "JetBrainsMono-2.304.zip"); if (result == 0) { printf("File downloaded
 * successfully.\n"); } else { fprintf(stderr, "Failed to download file.\n");
 *   }
 *
 * Notes:
 *   - Uses libcurl for handling HTTP requests and downloading files.
 *   - Requires proper initialization of libcurl environment and linking with
 * libcurl library.
 *   - The file is saved to the current working directory with the specified
 * filename.
 *   - Error handling is basic; it's recommended to enhance error checks as per
 * specific application requirements.
 *   - Ensure proper file permissions and network connectivity for successful
 * operation.
 */

// Callback function to write downloaded data to a file
static size_t mono_write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  return fwrite(ptr, size, nmemb, stream);
}

// Function to download a file from a URL using libcurl
int mono_download_file(const char *url, const char *output_filename) {
  CURL *curl;
  FILE *fp;
  CURLcode res;
    
  // Initialize libcurl
  curl = curl_easy_init();
  if (curl) {
    // Open file for writing
    fp = fopen(output_filename, "wb");
    if (fp == NULL) {
      fprintf(stderr, "Error opening file %s\n", output_filename);
      curl_easy_cleanup(curl);
      return 1;
    }
        
    // Set URL to download
    curl_easy_setopt(curl, CURLOPT_URL, url);
        
    // Set callback function to write data
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, mono_write_data);
        
    // Set file to write to
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
    // Perform the request
    res = curl_easy_perform(curl);
        
    // Check for errors
    if (res != CURLE_OK) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
      fclose(fp);
      curl_easy_cleanup(curl);
      return 1;
    }
        
    // Clean up
    fclose(fp);
    curl_easy_cleanup(curl);
  } else {
    fprintf(stderr, "Error initializing curl.\n");
    return 1;
  }
    
  return 0;
}

/**
 * mono_create_desktop_file Function
 *
 * Creates a minimal .desktop file with specified parameters for a desktop
 * application launcher.
 *
 * Parameters:
 *   - f (const char*): The filename (including path) of the .desktop file to
 *     create.
 *   - name (const char*): The name of the application to display in the launcher.
 *   - exec (const char*): The command to execute when the application is launched.
 *   - comment (const char*): A brief description or comment about the application.
 *   - icon (const char*): The path to the icon file to display for the application.
 *
 * Returns:
 *   - int: Returns 0 on success, non-zero on failure.
 *
 * Usage:
 *   int result = mono_create_desktop_file("myapp.desktop", "My Application",
 *                                         "/path/to/my/application",
 *                                         "A brief description of my application",
 *                                         "/path/to/my/icon.png");
 *   if (result == 0) {
 *       printf(".desktop file created successfully.\n");
 *   } else {
 *       fprintf(stderr, "Failed to create .desktop file.\n");
 *   }
 *
 * Notes:
 *   - The function creates a .desktop file conforming to the Desktop Entry
 *     Specification (https://specifications.freedesktop.org/desktop-entry-spec/latest/).
 *   - The `exec` parameter should be the full path to the executable or command
 *     to run the application.
 *   - The `icon` parameter specifies the path to an image file (typically a PNG
 *     or SVG) to use as the application's icon.
 *   - Ensure proper file permissions and directory write permissions for
 *     successful creation of the .desktop file.
 *   - Error handling is basic; it's recommended to enhance error checks as per
 *     specific application requirements.
 */
int mono_create_desktop_file(const char *f, const char *name, const char *exec, const char *comment, const char *icon) {
  FILE *fp;
  fp = fopen(f, "w");
  if (fp == NULL) {
    perror("Error creating .desktop file");
    mono_log(MONO_ERROR, "Error creating .desktop file");
    return 1; // Return an error code indicating failure
  }

  fprintf(fp, "[Desktop Entry]\n");
  fprintf(fp, "Version=1.0\n");
  fprintf(fp, "Name=%s\n", name);
  fprintf(fp, "Comment=%s\n", comment);
  fprintf(fp, "Exec=%s\n", exec);
  fprintf(fp, "Icon=%s\n", icon);
  fprintf(fp, "Terminal=false\n");
  fprintf(fp, "Type=Application\n");

  fclose(fp);

  return 0; // Return 0 to indicate success
}


#endif // UTILS_H_
