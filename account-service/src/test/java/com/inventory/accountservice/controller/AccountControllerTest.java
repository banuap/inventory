package com.inventory.accountservice.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.inventory.accountservice.dto.AccountDto;
import com.inventory.accountservice.entity.AccountType;
import com.inventory.accountservice.service.AccountService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(AccountController.class)
public class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AccountService accountService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    public void testGetAllAccounts() throws Exception {
        // Given
        AccountDto account1 = new AccountDto("John Doe", "123 Main St", "john@example.com", AccountType.SAVINGS);
        account1.setId(1L);
        AccountDto account2 = new AccountDto("Jane Smith", "456 Oak Ave", "jane@example.com", AccountType.CHECKING);
        account2.setId(2L);
        List<AccountDto> accounts = Arrays.asList(account1, account2);

        when(accountService.getAllAccounts()).thenReturn(accounts);

        // When & Then
        mockMvc.perform(get("/api/accounts"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.length()").value(2))
                .andExpect(jsonPath("$[0].name").value("John Doe"))
                .andExpect(jsonPath("$[1].name").value("Jane Smith"));
    }

    @Test
    public void testGetAccountById() throws Exception {
        // Given
        AccountDto account = new AccountDto("John Doe", "123 Main St", "john@example.com", AccountType.SAVINGS);
        account.setId(1L);

        when(accountService.getAccountById(1L)).thenReturn(Optional.of(account));

        // When & Then
        mockMvc.perform(get("/api/accounts/1"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.name").value("John Doe"))
                .andExpect(jsonPath("$.email").value("john@example.com"));
    }

    @Test
    public void testGetAccountById_NotFound() throws Exception {
        // Given
        when(accountService.getAccountById(999L)).thenReturn(Optional.empty());

        // When & Then
        mockMvc.perform(get("/api/accounts/999"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void testCreateAccount() throws Exception {
        // Given
        AccountDto newAccount = new AccountDto("John Doe", "123 Main St", "john@example.com", AccountType.SAVINGS);
        AccountDto createdAccount = new AccountDto("John Doe", "123 Main St", "john@example.com", AccountType.SAVINGS);
        createdAccount.setId(1L);

        when(accountService.createAccount(any(AccountDto.class))).thenReturn(createdAccount);

        // When & Then
        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(newAccount)))
                .andExpect(status().isCreated())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.id").value(1))
                .andExpect(jsonPath("$.name").value("John Doe"));
    }

    @Test
    public void testCreateAccount_ValidationError() throws Exception {
        // Given - account with missing required fields
        AccountDto invalidAccount = new AccountDto();
        invalidAccount.setName(""); // Empty name should fail validation

        // When & Then
        mockMvc.perform(post("/api/accounts")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(invalidAccount)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void testUpdateAccount() throws Exception {
        // Given
        AccountDto updatedAccount = new AccountDto("John Updated", "456 New St", "john.updated@example.com", AccountType.CHECKING);
        updatedAccount.setId(1L);

        when(accountService.updateAccount(eq(1L), any(AccountDto.class))).thenReturn(Optional.of(updatedAccount));

        // When & Then
        mockMvc.perform(put("/api/accounts/1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(updatedAccount)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.name").value("John Updated"));
    }

    @Test
    public void testDeleteAccount() throws Exception {
        // Given
        when(accountService.deleteAccount(1L)).thenReturn(true);

        // When & Then
        mockMvc.perform(delete("/api/accounts/1"))
                .andExpect(status().isNoContent());
    }

    @Test
    public void testDeleteAccount_NotFound() throws Exception {
        // Given
        when(accountService.deleteAccount(999L)).thenReturn(false);

        // When & Then
        mockMvc.perform(delete("/api/accounts/999"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void testGetServiceInfo() throws Exception {
        mockMvc.perform(get("/api/accounts/info"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.service").value("Account Opening Microservice"))
                .andExpect(jsonPath("$.version").value("1.0.0"));
    }
}