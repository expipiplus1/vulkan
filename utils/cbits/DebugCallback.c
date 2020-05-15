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
  int errorBitSet = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT & messageSeverity;
  int isError = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT == errorBitSet;

  fprintf(stderr, "Validation: %s\n", pCallbackData->pMessage);

  if (isError) {
    fprintf(stderr, "Aborting on validation error.\n");
    abort();
  }
  return VK_FALSE;
}
