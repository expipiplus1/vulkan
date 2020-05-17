#include <stdio.h>
#include <stdlib.h>
#include <vulkan/vulkan.h>

VKAPI_ATTR VkBool32 VKAPI_CALL
debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
              VkDebugUtilsMessageTypeFlagsEXT messageType,
              const VkDebugUtilsMessengerCallbackDataEXT *pCallbackData,
              void *pUserData) {
  fprintf(stderr, "Validation: %s\n", pCallbackData->pMessage);
  return VK_FALSE;
}

VKAPI_ATTR VkBool32 VKAPI_CALL
debugCallbackFatal(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
                   VkDebugUtilsMessageTypeFlagsEXT messageType,
                   const VkDebugUtilsMessengerCallbackDataEXT *pCallbackData,
                   void *pUserData) {
  debugCallback(messageSeverity, messageType, pCallbackData, pUserData);

  if (messageSeverity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) {
    fprintf(stderr, "Aborting on validation error.\n");
    abort();
  }
  return VK_FALSE;
}
