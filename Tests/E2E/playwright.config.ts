import { defineConfig } from "@playwright/test";
import path from "path";

export default defineConfig({
  testDir: "./",
  testMatch: ["*.spec.ts"],
  timeout: 60_000,
  use: {
    baseURL: "http://127.0.0.1:8000",
    headless: true,
  },
  webServer: {
    command: "./run",
    cwd: path.resolve(__dirname, "..", ".."),
    url: "http://127.0.0.1:8000",
    reuseExistingServer: true,
    timeout: 120_000,
  },
});
